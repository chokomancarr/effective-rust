use super::{Zero, Succ};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Either<L, R> {
    A(L),
    B(R),
}

pub trait Inject<T, Index> {
    fn inject(v: T) -> Self;
}

impl<T, Tail> Inject<T, Zero> for Either<T, Tail> {
    fn inject(v: T) -> Self {
        Either::A(v)
    }
}

impl<T, U, Index, Tail> Inject<T, Succ<Index>> for Either<U, Tail>
where
    Tail: Inject<T, Index>,
{
    fn inject(v: T) -> Self {
        Either::B(Tail::inject(v))
    }
}

pub trait Uninject<T, Index> {
    type Remainder;

    fn uninject(self) -> Result<T, Self::Remainder>;
}

impl<T, Tail> Uninject<T, Zero> for Either<T, Tail> {
    type Remainder = Tail;

    fn uninject(self) -> Result<T, Self::Remainder> {
        match self {
            Either::A(head) => Ok(head),
            Either::B(tail) => Err(tail),
        }
    }
}

impl<T, U, Tail, Index> Uninject<T, Succ<Index>> for Either<U, Tail>
where
    Tail: Uninject<T, Index>,
{
    type Remainder = Either<U, Tail::Remainder>;

    fn uninject(self) -> Result<T, Self::Remainder> {
        match self {
            Either::A(head) => Err(Either::A(head)),
            Either::B(tail) => tail.uninject().map_err(Either::B),
        }
    }
}

pub trait Select<T, Index> {
    fn get(self) -> Option<T>;
}

impl<T, Tail> Select<T, Zero> for Either<T, Tail> {
    fn get(self) -> Option<T> {
        match self {
            Either::A(head) => Some(head),
            Either::B(_) => None,
        }
    }
}

impl<T, U, Tail, Index> Select<T, Succ<Index>> for Either<U, Tail>
where
    Tail: Select<T, Index>,
{
    fn get(self) -> Option<T> {
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

impl<Target, Head, Tail, HeadIndex, TailIndices> Embed<Target, Either<HeadIndex, TailIndices>>
    for Either<Head, Tail>
where
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
    Subset<Either<Head, Tail>, Either<HeadIndex, TailIndices>> for T
where
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

impl<Head, Tail> Either<Head, Tail> {
    pub fn inject<T, Index>(v: T) -> Self
    where
        Self: Inject<T, Index>,
    {
        Inject::inject(v)
    }

    pub fn uninject<T, Index>(self) -> Result<T, <Self as Uninject<T, Index>>::Remainder>
    where
        Self: Uninject<T, Index>,
    {
        Uninject::uninject(self)
    }

    pub fn get<T, Index>(self) -> Option<T>
    where
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
    #[test]
    fn test_into_coproduct() {
        use crate::Coproduct;
        type Coprod = Coproduct![i32, u64, String,];

        let coproduct = Coprod::inject(42_i32);
        let coproduct = coproduct.as_ref();

        assert_eq!(coproduct.get::<&i32, _>(), Some(&42));
        assert_eq!(coproduct.get::<&u64, _>(), None);
        assert_eq!(coproduct.get::<&String, _>(), None);
    }

    #[test]
    fn test_embed_subset() {
        use crate::Coproduct;
        type Coprod = Coproduct![i32,];

        let v = Coprod::inject(42);

        let v2: Coproduct![i32, u64] = v.embed();
        let v3: Coproduct![u64, i32] = v2.embed();

        assert_eq!(v3.as_ref().get::<&i32, _>(), Some(&42));

        let v4: Result<Coproduct![u64], Coproduct![i32]> = v3.subset();

        assert_eq!(v4, Err(v));
    }
}
