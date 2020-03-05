module BoundedSet exposing
    ( BoundedSet
    , empty
    , insert
    , member
    , remove
    )


type BoundedSet a
    = BoundedSet Int (List a)


insert : a -> BoundedSet a -> BoundedSet a
insert value ((BoundedSet maxSize entries) as set) =
    if List.member value entries then
        set

    else
        BoundedSet maxSize (value :: List.take (maxSize - 1) entries)


remove : a -> BoundedSet a -> BoundedSet a
remove value (BoundedSet maxSize entries) =
    BoundedSet maxSize (List.filter (\entry -> entry /= value) entries)


empty : Int -> BoundedSet a
empty maxSize =
    BoundedSet maxSize []


member : a -> BoundedSet a -> Bool
member value (BoundedSet _ entries) =
    List.member value entries
