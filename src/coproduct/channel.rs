use crate::Effect;
use std::fmt;
use std::hash::{Hash, Hasher};

use super::{Zero, Succ};

pub enum Either<L: Effect, R> {
    A(L::Output),
    B(R),
}

impl<L, R> fmt::Debug for Either<L, R>
where
    L: Effect,
    L::Output: fmt::Debug,
    R: fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Either::*;
        match self {
            A(x) => f.debug_tuple("A")
                .field(x)
                .finish(),
            B(x) => f.debug_tuple("B")
                .field(x)
                .finish(),
        }
    }
}

impl<L, R> Clone for Either<L, R>
where
    L: Effect,
    L::Output: Clone,
    R: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        use self::Either::*;
        match self {
            A(x) => A(x.clone()),
            B(x) => B(x.clone()),
        }
    }
}

impl<L, R> Copy for Either<L, R>
where
    L: Effect,
    L::Output: Copy,
    R: Copy,
{}

impl<L, R> PartialEq for Either<L, R>
where
    L: Effect,
    L::Output: PartialEq,
    R: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        use self::Either::*;
        match (self, other) {
            (A(x), A(y)) => x == y,
            (B(x), B(y)) => x == y,
            _ => false,
        }
    }
}

impl<L, R> Eq for Either<L, R>
where
    L: Effect,
    L::Output: Eq,
    R: Eq,
{}

impl<L, R> Hash for Either<L, R>
where
    L: Effect,
    L::Output: Hash,
    R: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::Either::*;
        match self {
            A(x) => x.hash(state),
            B(x) => x.hash(state),
        }
    }
}

pub trait Inject<T: Effect, Index> {
    fn inject(v: T::Output) -> Self;
}

impl<T: Effect, Tail> Inject<T, Zero> for Either<T, Tail> {
    fn inject(v: T::Output) -> Self {
        Either::A(v)
    }
}

impl<T, U, Index, Tail> Inject<T, Succ<Index>> for Either<U, Tail>
where
    T: Effect,
    U: Effect,
    Tail: Inject<T, Index>,
{
    fn inject(v: T::Output) -> Self {
        Either::B(Tail::inject(v))
    }
}

pub trait Uninject<T: Effect, Index> {
    type Remainder;

    fn uninject(self) -> Result<T::Output, Self::Remainder>;
}

impl<T: Effect, Tail> Uninject<T, Zero> for Either<T, Tail> {
    type Remainder = Tail;

    fn uninject(self) -> Result<T::Output, Self::Remainder> {
        match self {
            Either::A(head) => Ok(head),
            Either::B(tail) => Err(tail),
        }
    }
}

impl<T, U, Tail, Index> Uninject<T, Succ<Index>> for Either<U, Tail>
where
    T: Effect,
    U: Effect,
    Tail: Uninject<T, Index>,
{
    type Remainder = Either<U, Tail::Remainder>;

    fn uninject(self) -> Result<T::Output, Self::Remainder> {
        match self {
            Either::A(head) => Err(Either::A(head)),
            Either::B(tail) => tail.uninject().map_err(Either::B),
        }
    }
}

pub trait Select<T: Effect, Index> {
    fn get(self) -> Option<T::Output>;
}

impl<T: Effect, Tail> Select<T, Zero> for Either<T, Tail> {
    fn get(self) -> Option<T::Output> {
        match self {
            Either::A(head) => Some(head),
            Either::B(_) => None,
        }
    }
}

impl<T, U, Tail, Index> Select<T, Succ<Index>> for Either<U, Tail>
where
    T: Effect,
    U: Effect,
    Tail: Select<T, Index>,
{
    fn get(self) -> Option<T::Output> {
        match self {
            Either::A(_) => None,
            Either::B(tail) => tail.get(),
        }
    }
}

pub trait AsRef<'a> {
    type Output;

    fn as_ref(&'a self) -> Self::Output;
}

impl<'a> AsRef<'a> for ! {
    type Output = !;

    fn as_ref(&'a self) -> Self::Output {
        match *self {}
    }
}

impl<'a, Head: 'a, Tail> AsRef<'a> for Either<Head, Tail>
where
    Head: Effect,
    Tail: AsRef<'a>,
{
    type Output = Either<&'a Head, Tail::Output>;

    fn as_ref(&'a self) -> Self::Output {
        match self {
            Either::A(ref head) => Either::A(head),
            Either::B(ref tail) => Either::B(tail.as_ref()),
        }
    }
}

pub trait AsMut<'a> {
    type Output;

    fn as_mut(&'a mut self) -> Self::Output;
}

impl<'a> AsMut<'a> for ! {
    type Output = !;

    fn as_mut(&'a mut self) -> Self::Output {
        match *self {}
    }
}

impl<'a, Head: 'a, Tail> AsMut<'a> for Either<Head, Tail>
where
    Head: Effect,
    Tail: AsMut<'a>,
{
    type Output = Either<&'a mut Head, Tail::Output>;

    fn as_mut(&'a mut self) -> Self::Output {
        match self {
            Either::A(ref mut head) => Either::A(head),
            Either::B(ref mut tail) => Either::B(tail.as_mut()),
        }
    }
}

pub trait Embed<Target, Indices> {
    fn embed(self) -> Target;
}

impl<Target> Embed<Target, !> for ! {
    fn embed(self) -> Target {
        match self {}
    }
}

impl<Target, Head, Tail, HeadIndex, TailIndices> Embed<Target, super::eff::Either<HeadIndex, TailIndices>>
    for Either<Head, Tail>
where
    Head: Effect,
    Target: Inject<Head, HeadIndex>,
    Tail: Embed<Target, TailIndices>,
{
    fn embed(self) -> Target {
        match self {
            Either::A(head) => Inject::inject(head),
            Either::B(tail) => tail.embed(),
        }
    }
}

pub trait Subset<Target, Indices> {
    type Remainder;

    fn subset(self) -> Result<Target, Self::Remainder>;
}

impl<T> Subset<!, !> for T {
    type Remainder = T;

    fn subset(self) -> Result<!, Self::Remainder> {
        Err(self)
    }
}

impl<T, Head, Tail, HeadIndex, TailIndices, Rem>
    Subset<Either<Head, Tail>, super::eff::Either<HeadIndex, TailIndices>> for T
where
    Head: Effect,
    T: Uninject<Head, HeadIndex, Remainder = Rem>,
    Rem: Subset<Tail, TailIndices>,
{
    type Remainder = Rem::Remainder;

    fn subset(self) -> Result<Either<Head, Tail>, Self::Remainder> {
        match self.uninject() {
            Ok(v) => Ok(Either::A(v)),
            Err(rem) => rem.subset().map(Either::B),
        }
    }
}

impl<Head: Effect, Tail> Either<Head, Tail> {
    pub fn inject<T, Index>(v: T::Output) -> Self
    where
        T: Effect,
        Self: Inject<T, Index>,
    {
        Inject::inject(v)
    }

    pub fn uninject<T, Index>(self) -> Result<T::Output, <Self as Uninject<T, Index>>::Remainder>
    where
        T: Effect,
        Self: Uninject<T, Index>,
    {
        Uninject::uninject(self)
    }

    pub fn get<T, Index>(self) -> Option<T::Output>
    where
        T: Effect,
        Self: Select<T, Index>,
    {
        Select::get(self)
    }

    pub fn as_ref<'a>(&'a self) -> <Self as AsRef<'a>>::Output
    where
        Self: AsRef<'a>,
    {
        AsRef::as_ref(self)
    }

    pub fn as_mut<'a>(&'a mut self) -> <Self as AsMut<'a>>::Output
    where
        Self: AsMut<'a>,
    {
        AsMut::as_mut(self)
    }

    pub fn embed<Target, Indices>(self) -> Target
    where
        Self: Embed<Target, Indices>,
    {
        Embed::embed(self)
    }

    pub fn subset<Target, Indices>(
        self,
    ) -> Result<Target, <Self as Subset<Target, Indices>>::Remainder>
    where
        Self: Subset<Target, Indices>,
    {
        Subset::subset(self)
    }
}

#[cfg(test)]
mod test {
    #[derive(Debug)]
    struct Foo;

    impl crate::Effect for Foo {
        type Output = u32;
    }

    #[derive(Debug)]
    struct Bar;

    impl crate::Effect for Bar {
        type Output = u32;
    }

    #[derive(Debug)]
    struct Baz;

    impl crate::Effect for Baz {
        type Output = u32;
    }

    fn ensure_uninject<C, E, Index>()
    where
        C: super::Uninject<E, Index>,
        E: crate::Effect,
    {
    }

    #[test]
    fn test_into_coproduct() {
        type Coprod = crate::CoproductChannel![Foo, Bar, Baz,];

        ensure_uninject::<Coprod, Foo, super::Zero>();
        ensure_uninject::<Coprod, Bar, super::Succ<super::Zero>>();
        ensure_uninject::<Coprod, Baz, super::Succ<super::Succ<super::Zero>>>();
        
        let coproduct = Coprod::inject::<Foo, _>(42);
        let c = coproduct.as_ref();

        assert_eq!(c.get::<&Foo, _>(), Some(&42));
        assert_eq!(c.get::<&Bar, _>(), None);
        assert_eq!(c.get::<&Baz, _>(), None);

        assert_eq!(coproduct.uninject::<Foo, _>(), Ok(42));
    }

    #[test]
    fn test_embed_subset() {
        use crate::CoproductChannel as Coproduct;
        type Coprod = Coproduct![Foo,];

        let v = Coprod::inject(42);

        let v2: Coproduct![Foo, Bar] = v.embed();
        let v3: Coproduct![Bar, Foo] = v2.embed();

        assert_eq!(v3.as_ref().get::<&Foo, _>(), Some(&42));

        let v4: Result<Coproduct![Bar], Coproduct![Foo]> = v3.subset();

        assert_eq!(v4, Err(v));
    }
}
