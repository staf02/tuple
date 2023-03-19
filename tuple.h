#pragma once

#include <algorithm>
#include <cstddef>
#include <memory>
#include <type_traits>

template <typename... Rest>
class tuple;

template <size_t Index>
class in_place_index_t {
public:
    explicit constexpr in_place_index_t() = default;
};

template <size_t Index>
inline constexpr in_place_index_t<Index> in_place_index;

template <std::size_t I, class T>
struct tuple_element;

template <size_t Index, typename First, typename... Rest>
struct tuple_element<Index, tuple<First, Rest...>>
    : tuple_element<Index - 1, tuple<Rest...>> {};

template <typename First, typename... Rest>
struct tuple_element<0, tuple<First, Rest...>> {
    using type = First;
};

template <size_t Index, typename Tuple>
using tuple_element_t = typename tuple_element<Index, Tuple>::type;

template <size_t Index, typename Tuple>
struct tuple_element<Index, const Tuple> {
    using type = std::add_const_t<tuple_element_t<Index, Tuple>>;
};

template <size_t Index, typename Tuple>
struct tuple_element<Index, volatile Tuple> {
    using type = std::add_volatile_t<tuple_element_t<Index, Tuple>>;
};

template <size_t Index, typename Tuple>
struct tuple_element<Index, const volatile Tuple> {
    using type = std::add_cv_t<tuple_element_t<Index, Tuple>>;
};

namespace tuple_impl {
    template <typename... Types>
    concept default_ctor = (std::is_default_constructible_v<Types> && ...);
}

template <>
class tuple<> {
public:
    void swap(tuple& other) {}
};

template <typename First, typename... Rest>
class tuple<First, Rest...> : public tuple<Rest...> {
public:
    using rest = tuple<Rest...>;
    using rest::rest;

    constexpr tuple() requires(tuple_impl::default_ctor<First, Rest...>)
        : first() {};

    constexpr tuple()
        requires(!(std::is_default_constructible_v<First>)) = delete;

    constexpr tuple(const Rest&... args)
        requires(sizeof...(Rest) >= 1 && (std::is_constructible_v<Rest> && ...))
    : tuple(std::forward<Rest>(args)...) {}

    tuple(const tuple& other) = default;
    tuple(tuple&& other) = default;
    tuple& operator=(const tuple& other) = default;
    tuple& operator=(tuple&& other) = default;

    template <typename UFirst, typename... URest>
    constexpr tuple(UFirst&& arg, URest&&... args)
        requires(std::conjunction_v<
            std::bool_constant<sizeof...(URest) == sizeof...(Rest)>,
                std::negation<std::is_same<std::remove_cvref_t<UFirst>, tuple>>,
                std::is_constructible<First, UFirst>>)
        : first(std::move(arg)), rest(std::forward<URest>(args)...) {}

            template <typename UFirst, typename... URest>
            constexpr tuple(const tuple<UFirst, URest...>& other)
                : first(other.first), rest(*static_cast<tuple<URest...> const*>(&other)) {
            }

            template <typename UFirst, typename... URest>
            constexpr tuple(tuple<UFirst, URest...>&& other)
                : first(std::move(other.first)),
                rest(std::move(*static_cast<tuple<URest...>*>(&other))) {}

            template <typename... Args>
            friend struct tuple;

            template <std::size_t N, typename... gTypes>
            friend constexpr tuple_element_t<N, tuple<gTypes...>>&
                get(tuple<gTypes...>& t) noexcept;

            template <std::size_t N, typename... gTypes>
            friend constexpr tuple_element_t<N, tuple<gTypes...>>&&
                get(tuple<gTypes...>&& t) noexcept;

            template <std::size_t N, typename... gTypes>
            friend constexpr const tuple_element_t<N, tuple<gTypes...>>&
                get(const tuple<gTypes...>& t) noexcept;

            template <std::size_t N, typename... gTypes>
            friend constexpr const tuple_element_t<N, tuple<gTypes...>>&&
                get(const tuple<gTypes...>&& t) noexcept;

            void swap(tuple& other) {
                using std::swap;
                std::swap(first, other.first);
                static_cast<rest*>(this)->swap(*static_cast<rest*>(&other));
            }

private:
    template <size_t Index>
    constexpr auto& get(in_place_index_t<Index>) {
        return static_cast<rest*>(this)->get(in_place_index<Index - 1>);
    }

    constexpr auto& get(in_place_index_t<0>) {
        return first;
    }

    template <size_t Index>
    constexpr auto const& get(in_place_index_t<Index>) const {
        return static_cast<rest const*>(this)->get(in_place_index<Index - 1>);
    }

    constexpr auto const& get(in_place_index_t<0>) const {
        return first;
    }

    First first;
};

template <class... Rest>
constexpr tuple<std::unwrap_ref_decay_t<Rest>...> make_tuple(Rest&&... args) {
    return tuple<std::unwrap_ref_decay_t<Rest>...>(std::forward<Rest>(args)...);
}

namespace tuple_impl {
    template <bool is_same, typename CurrentType, typename... Rest>
    struct index_chooser {
        static constexpr size_t Index = 0;
    };

    template <typename CurrentType, typename T, typename... Rest>
    struct index_chooser<false, CurrentType, T, Rest...> {
        static constexpr size_t Index = index_chooser<std::is_same_v<T, CurrentType>,
            CurrentType, Rest...>::Index +
            1;
    };

    template <typename CurrentType, typename T, typename... Rest>
    inline constexpr size_t index_chooser_v =
        index_chooser<std::is_same_v<T, CurrentType>, CurrentType, Rest...>::Index;

    template <typename T, typename... Rest>
    inline constexpr bool is_unique_type_v = (std::is_same_v<T, Rest> +...) == 1;
} // namespace tuple_impl

template <typename... Rest>
struct tuple_size;

template <typename Tuple>
struct tuple_size<const Tuple> : tuple_size<Tuple> {};

template <typename Tuple>
struct tuple_size<volatile Tuple> : tuple_size<Tuple> {};

template <typename Tuple>
struct tuple_size<const volatile Tuple> : tuple_size<Tuple> {};

template <typename... Rest>
struct tuple_size<tuple<Rest...>>
    : std::integral_constant<size_t, sizeof...(Rest)> {};

template <typename Tuple>
inline constexpr size_t tuple_size_v = tuple_size<Tuple>::value;

template <std::size_t N, typename... Rest>
constexpr tuple_element_t<N, tuple<Rest...>>& get(tuple<Rest...>& t) noexcept {
    return t.get(in_place_index<N>);
}

template <std::size_t N, typename... Rest>
constexpr tuple_element_t<N, tuple<Rest...>>&&
get(tuple<Rest...>&& t) noexcept {
    return std::move(get<N>(t));
}

template <std::size_t N, typename... Rest>
constexpr const tuple_element_t<N, tuple<Rest...>>&
get(const tuple<Rest...>& t) noexcept {
    return t.get(in_place_index<N>);
}

template <std::size_t N, typename... Rest>
constexpr const tuple_element_t<N, tuple<Rest...>>&&
get(const tuple<Rest...>&& t) noexcept {
    return std::move(get<N>(t));
}

template <typename T, typename... Rest>
    requires(tuple_impl::is_unique_type_v<T, Rest...>) constexpr T& get(
tuple<Rest...>& t) noexcept {
    return get<tuple_impl::index_chooser_v<T, Rest...>>(t);
}

template <typename T, typename... Rest>
    requires(tuple_impl::is_unique_type_v<T, Rest...>) constexpr T&& get(
tuple<Rest...>&& t) noexcept {
    return std::move(get<tuple_impl::index_chooser_v<T, Rest...>>(t));
}

template <typename T, typename... Rest>
    requires(tuple_impl::is_unique_type_v<T, Rest...>) constexpr const T& get(
const tuple<Rest...>& t) noexcept {
    return get<tuple_impl::index_chooser_v<T, Rest...>>(t);
}

template <typename T, typename... Rest>
    requires(tuple_impl::is_unique_type_v<T, Rest...>) constexpr const T&& get(
const tuple<Rest...>&& t) noexcept {
    return std::move(get<tuple_impl::index_chooser_v<T, Rest...>>(t));
}

// swap specialization
template <typename... TTypes, typename... URest>
void swap(tuple<TTypes...>& first, tuple<URest...>& second) {
    first.swap(second);
}

// compare operators

namespace tuple_impl {
    template <size_t Index>
    struct tuple_el_equals {
        template <class TType, class UType>
        constexpr bool operator()(const TType& x, const UType& y) {
            return get<Index - 1>(x) == get<Index - 1>(y) &&
                tuple_el_equals<Index - 1>()(x, y);
        }
    };

    template <>
    struct tuple_el_equals<0> {
        template <class TType, class UType>
        constexpr bool operator()(const TType&, const UType&) {
            return true;
        }
    };

    template <size_t Index>
    struct tuple_el_less {
        template <class TType, class UType>
        bool operator()(const TType& x, const UType& y) {
            const size_t current_index = tuple_size<TType>::value - Index;
            if (get<current_index>(x) < get<current_index>(y)) {
                return true;
            }
            else if (get<current_index>(y) < get<current_index>(x)) {
                return false;
            }
            return tuple_el_less<Index - 1>()(x, y);
        }
    };

    template <>
    struct tuple_el_less<0> {
        template <class TType, class UType>
        bool operator()(const TType&, const UType&) {
            return false;
        }
    };
} // namespace tuple_impl

template <class... TTypes, class... URest>
constexpr bool operator==(const tuple<TTypes...>& x, const tuple<URest...>& y) {
    static_assert(sizeof...(TTypes) == sizeof...(URest),
        "Tuples with different size are not comparable");
    return tuple_impl::tuple_el_equals<sizeof...(TTypes)>()(x, y);
}

template <class... TTypes, class... URest>
constexpr bool operator<(const tuple<TTypes...>& x, const tuple<URest...>& y) {
    static_assert(sizeof...(TTypes) == sizeof...(URest),
        "Tuples with different size are not comparable");
    return tuple_impl::tuple_el_less<sizeof...(TTypes)>()(x, y);
}

template <typename... TTypes, typename... URest>
constexpr bool operator!=(const tuple<TTypes...>& first,
    const tuple<URest...>& second) {
    return !(first == second);
}

template <typename... TTypes, typename... URest>
constexpr bool operator<=(const tuple<TTypes...>& first,
    const tuple<URest...>& second) {
    return !(second < first);
}

template <typename... TTypes, typename... URest>
constexpr bool operator>(const tuple<TTypes...>& first,
    const tuple<URest...>& second) {
    return !(first <= second);
}

template <typename... TTypes, typename... URest>
constexpr bool operator>=(const tuple<TTypes...>& first,
    const tuple<URest...>& second) {
    return !(first < second);
}
