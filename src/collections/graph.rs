use core::marker::PhantomData;

use typenum::U0;

use crate::{
    cmp::IsEqual,
    num::Addition,
    traits::{
        fold::Foldable,
        functor::{Map, Mapper},
    },
};

use super::{
    Container,
    list::{self, Empty, Filter, IntoOnes, Len, List, Ones, UntupleLeft, UntupleRight, ZipTuple},
    record::{self, Record},
    set::{self, Set},
    tuple,
};

pub struct Graph<Nodes, IncomingEdges, OutgoingEdges>(
    PhantomData<Nodes>,
    PhantomData<IncomingEdges>,
    PhantomData<OutgoingEdges>,
);

pub trait InsertNode {
    type Out;
}
impl<Id, Data, IncomingEdges, OutgoingEdges, Nodes> InsertNode
    for (Graph<Nodes, IncomingEdges, OutgoingEdges>, (Id, Data))
where
    (Nodes, (Id, Data)): record::InsertField,
    (IncomingEdges, (Id, Set<Empty>)): record::InsertField,
    (OutgoingEdges, (Id, Set<Empty>)): record::InsertField,
{
    type Out = Graph<
        <(Nodes, (Id, Data)) as record::InsertField>::Out,
        <(IncomingEdges, (Id, Set<Empty>)) as record::InsertField>::Out,
        <(OutgoingEdges, (Id, Set<Empty>)) as record::InsertField>::Out,
    >;
}
pub type Insert<T, Node> = <(T, Node) as InsertNode>::Out;

pub struct ExcludeIds<Id>(PhantomData<Id>);
impl<K, T, Id> Mapper<(K, Set<T>)> for ExcludeIds<Id>
where
    (Set<T>, Id): set::Difference,
{
    type Out = (K, <(Set<T>, Id) as set::Difference>::Out);
}
pub type Excluded<T, U> =
    <(T, ExcludeIds<U>) as Map<<T as Container>::Content, ExcludeIds<U>>>::Out;

pub trait RemoveNode {
    type Out;
}
impl<Id, In, Out, Nodes> RemoveNode for (Graph<Nodes, In, Out>, Id)
where
    (Graph<Nodes, In, Out>, Set<List<(Id, list::Empty)>>): RemoveNodes,
{
    type Out = <(Graph<Nodes, In, Out>, Set<List<(Id, list::Empty)>>) as RemoveNodes>::Out;
}
pub type Remove<T, Node> = <(T, Node) as RemoveNode>::Out;

pub trait RemoveNodes {
    type Out;
}
impl<Id, Ids1, Ids2, In, Out, Nodes> RemoveNodes
    for (
        Graph<Nodes, Record<Set<Ids1>, In>, Record<Set<Ids2>, Out>>,
        Id,
    )
where
    (Nodes, Id): record::RemoveKeys,
    In: Container,
    (In, ExcludeIds<Id>): Map<<In as Container>::Content, ExcludeIds<Id>>,
    (Record<Set<Ids1>, Excluded<In, Id>>, Id): record::RemoveKeys,
    Out: Container,
    (Out, ExcludeIds<Id>): Map<<Out as Container>::Content, ExcludeIds<Id>>,
    (Record<Set<Ids2>, Excluded<Out, Id>>, Id): record::RemoveKeys,
{
    type Out = Graph<
        <(Nodes, Id) as record::RemoveKeys>::Out,
        <(Record<Set<Ids1>, Excluded<In, Id>>, Id) as record::RemoveKeys>::Out,
        <(Record<Set<Ids2>, Excluded<Out, Id>>, Id) as record::RemoveKeys>::Out,
    >;
}

pub trait ConnectNodes {
    type Out;
}
impl<FromId, ToId, IncomingEdges, OutgoingEdges, Nodes> ConnectNodes
    for (Graph<Nodes, IncomingEdges, OutgoingEdges>, (FromId, ToId))
where
    (OutgoingEdges, FromId): record::GetEntry,
    (<(OutgoingEdges, FromId) as record::GetEntry>::Out, ToId): set::Insert,
    (OutgoingEdges, FromId): record::RemoveEntry,
    (
        <(OutgoingEdges, FromId) as record::RemoveEntry>::Out,
        (
            FromId,
            <(<(OutgoingEdges, FromId) as record::GetEntry>::Out, ToId) as set::Insert>::Out,
        ),
    ): record::InsertField,
    (IncomingEdges, ToId): record::GetEntry,
    (<(IncomingEdges, ToId) as record::GetEntry>::Out, FromId): set::Insert,
    (IncomingEdges, ToId): record::RemoveEntry,
    (
        <(IncomingEdges, ToId) as record::RemoveEntry>::Out,
        (
            ToId,
            <(<(IncomingEdges, ToId) as record::GetEntry>::Out, FromId) as set::Insert>::Out,
        ),
    ): record::InsertField,
{
    type Out = Graph<
        Nodes,
        <(
            <(IncomingEdges, ToId) as record::RemoveEntry>::Out,
            (
                ToId,
                <(<(IncomingEdges, ToId) as record::GetEntry>::Out, FromId) as set::Insert>::Out,
            ),
        ) as record::InsertField>::Out,
        <(
            <(OutgoingEdges, FromId) as record::RemoveEntry>::Out,
            (
                FromId,
                <(<(OutgoingEdges, FromId) as record::GetEntry>::Out, ToId) as set::Insert>::Out,
            ),
        ) as record::InsertField>::Out,
    >;
}
pub type Connect<Graph, From, To> = <(Graph, (From, To)) as ConnectNodes>::Out;

pub trait DisconnectNodes {
    type Out;
}
impl<FromId, ToId, IncomingEdges, OutgoingEdges, Nodes> DisconnectNodes
    for (Graph<Nodes, IncomingEdges, OutgoingEdges>, (FromId, ToId))
where
    (OutgoingEdges, FromId): record::GetEntry,
    (<(OutgoingEdges, FromId) as record::GetEntry>::Out, ToId): set::Remove,
    (OutgoingEdges, FromId): record::RemoveEntry,
    (
        <(OutgoingEdges, FromId) as record::RemoveEntry>::Out,
        (
            FromId,
            <(<(OutgoingEdges, FromId) as record::GetEntry>::Out, ToId) as set::Remove>::Out,
        ),
    ): record::InsertField,
    (IncomingEdges, ToId): record::GetEntry,
    (<(IncomingEdges, ToId) as record::GetEntry>::Out, FromId): set::Remove,
    (IncomingEdges, ToId): record::RemoveEntry,
    (
        <(IncomingEdges, ToId) as record::RemoveEntry>::Out,
        (
            ToId,
            <(<(IncomingEdges, ToId) as record::GetEntry>::Out, FromId) as set::Remove>::Out,
        ),
    ): record::InsertField,
{
    type Out = Graph<
        Nodes,
        <(
            <(IncomingEdges, ToId) as record::RemoveEntry>::Out,
            (
                ToId,
                <(<(IncomingEdges, ToId) as record::GetEntry>::Out, FromId) as set::Remove>::Out,
            ),
        ) as record::InsertField>::Out,
        <(
            <(OutgoingEdges, FromId) as record::RemoveEntry>::Out,
            (
                FromId,
                <(<(OutgoingEdges, FromId) as record::GetEntry>::Out, ToId) as set::Remove>::Out,
            ),
        ) as record::InsertField>::Out,
    >;
}
pub type Disconnect<Graph, From, To> = <(Graph, (From, To)) as DisconnectNodes>::Out;

pub trait ListOutgoing {
    type Out;
}
impl<Id, In, Out, Nodes> ListOutgoing for (Graph<Nodes, In, Out>, Id)
where
    (Out, Id): record::GetEntry,
{
    type Out = record::Get<Out, Id>;
}
pub type Outgoing<Graph, Id> = <(Graph, Id) as ListOutgoing>::Out;

pub trait ListIncoming {
    type Out;
}
impl<Id, In, Out, Nodes> ListIncoming for (Graph<Nodes, In, Out>, Id)
where
    (In, Id): record::GetEntry,
{
    type Out = record::Get<In, Id>;
}
pub type Incoming<Graph, Id> = <(Graph, Id) as ListIncoming>::Out;

pub trait Size {
    type Out;
}
impl<Nodes, IncomingEdges, OutgoingEdges> Size for Graph<Nodes, IncomingEdges, OutgoingEdges>
where
    Nodes: record::Size,
{
    type Out = <Nodes as record::Size>::Out;
}

pub struct EdgesEmpty;
impl<T> Mapper<Set<T>> for EdgesEmpty
where
    T: Container,
    (T, Ones): Map<<T as Container>::Content, Ones>,
    IntoOnes<T>: Foldable<Addition>,
    (Len<T>, U0): IsEqual,
{
    type Out = <(Len<T>, U0) as IsEqual>::Out;
}

pub trait Sources {
    type Out;
}
impl<Nodes, Ids, In, Out> Sources for Graph<Nodes, Record<Ids, In>, Out>
where
    In: Container,
    (In, tuple::Left): Map<<In as Container>::Content, tuple::Left>,
    (In, tuple::Right): Map<<In as Container>::Content, tuple::Right>,
    UntupleRight<In>: Container,
    (UntupleRight<In>, EdgesEmpty): Map<<UntupleRight<In> as Container>::Content, EdgesEmpty>,
    (
        UntupleLeft<In>,
        <(UntupleRight<In>, EdgesEmpty) as Map<
            <UntupleRight<In> as Container>::Content,
            EdgesEmpty,
        >>::Out,
    ): ZipTuple,
    <(
        UntupleLeft<In>,
        <(UntupleRight<In>, EdgesEmpty) as Map<
            <UntupleRight<In> as Container>::Content,
            EdgesEmpty,
        >>::Out,
    ) as ZipTuple>::Out: Foldable<Filter>,
{
    type Out = Set<
        <<(
            UntupleLeft<In>,
            <(UntupleRight<In>, EdgesEmpty) as Map<
                <UntupleRight<In> as Container>::Content,
                EdgesEmpty,
            >>::Out,
        ) as ZipTuple>::Out as Foldable<Filter>>::Out,
    >;
}

pub trait Sinks {
    type Out;
}
impl<Nodes, In, Out> Sinks for Graph<Nodes, In, Out>
where
    Graph<Nodes, In, Out>: Transpose,
    <Graph<Nodes, In, Out> as Transpose>::Out: Sources,
{
    type Out = <<Graph<Nodes, In, Out> as Transpose>::Out as Sources>::Out;
}

pub trait Transpose {
    type Out;
}
impl<Nodes, In, Out> Transpose for Graph<Nodes, In, Out> {
    type Out = Graph<Nodes, Out, In>;
}

pub trait Source {
    type Out;
}
impl<Nodes, In, Out> Source for Graph<Nodes, In, Out>
where
    Graph<Nodes, In, Out>: Sources,
    <Graph<Nodes, In, Out> as Sources>::Out: Container,
{
    type Out = <<Graph<Nodes, In, Out> as Sources>::Out as Container>::Content;
}

pub trait GetData {
    type Out;
}
impl<Id, Nodes, In, Out> GetData for (Graph<Nodes, In, Out>, Id)
where
    (Nodes, Id): record::GetEntry,
{
    type Out = <(Nodes, Id) as record::GetEntry>::Out;
}
pub type Get<T, Id> = <(T, Id) as GetData>::Out;

pub trait Topological {
    type Out;
}
impl Topological for Graph<record::Empty, record::Empty, record::Empty> {
    type Out = list::Empty;
}
type Ids<H, T> = Set<List<(H, T)>>;
impl<Nodes, Ids1h, Ids1t, Ids2h, Ids2t, In, Out> Topological
    for Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>>
where
    In: Container,
    (In, tuple::Left): Map<<In as Container>::Content, tuple::Left>,
    (In, tuple::Right): Map<<In as Container>::Content, tuple::Right>,
    UntupleRight<In>: Container,
    (UntupleRight<In>, EdgesEmpty): Map<<UntupleRight<In> as Container>::Content, EdgesEmpty>,
    (
        UntupleLeft<In>,
        <(UntupleRight<In>, EdgesEmpty) as Map<
            <UntupleRight<In> as Container>::Content,
            EdgesEmpty,
        >>::Out,
    ): ZipTuple,
    <(
        UntupleLeft<In>,
        <(UntupleRight<In>, EdgesEmpty) as Map<
            <UntupleRight<In> as Container>::Content,
            EdgesEmpty,
        >>::Out,
    ) as ZipTuple>::Out: Foldable<Filter>,
    Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>>: Sources,
    (
        Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>>,
        <Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>> as Sources>::Out,
    ): RemoveNodes,
    <(
        Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>>,
        <Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>> as Sources>::Out,
    ) as RemoveNodes>::Out: Topological,
{
    type Out = List<(
        <Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>> as Sources>::Out,
        <<(
            Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>>,
            <Graph<Nodes, Record<Ids<Ids1h, Ids1t>, In>, Record<Ids<Ids2h, Ids2t>, Out>> as Sources>::Out,
        ) as RemoveNodes>::Out as Topological>::Out,
    )>;
}
pub type Topo<G> = <G as Topological>::Out;

#[macro_export]
macro_rules! graph {
    {} => {
        $crate::collections::graph::Graph<$crate::record!{}, $crate::record!{}, $crate::record!{}>
    };
    {($id:ty,$data:ty):[$($edges:ty),*]} => {
        <($crate::graph!{}, ($id, $data)) as $crate::collections::graph::InsertNode>::Out
    };
    {($id:ty,$data:ty):[$($edges:ty),*],$(($ids:ty,$datas:ty):[$($edgeses:ty),*]),+} => {
        $crate::connect_graph!(<($crate::graph!{$(($ids,$datas):[]),+}, ($id, $data)) as $crate::collections::graph::InsertNode>::Out,$id:[$($edges),*],$($ids:[$($edgeses),*]),*)
    };
}

#[macro_export]
macro_rules! connect_graph {
    ($graph:ty,$id:ty:[]) => {
        $graph
    };
    ($graph:ty,$from:ty:[$to:ty]) => {
        <($graph, ($from, $to)) as $crate::collections::graph::ConnectNodes>::Out
    };
    ($graph:ty,$id:ty:[$to:ty,$($edges:ty),+]) => {
        $crate::connect_graph!($crate::connect_graph!($graph, $id: [$to]),$id:[$($edges),*])
    };
    ($graph:ty,$id:ty:[$($edges:ty),*]$($toks:tt)*) => {
        $crate::connect_graph!($crate::connect_graph!($graph,$id:[$($edges),*])$($toks)*)
    };
}

#[allow(unused)]
#[cfg(test)]
mod test {
    use typenum::U4;

    use super::*;
    use crate::collections::list::Head;
    use crate::dinosaurs::*;
    use crate::num::consts::{U0, U1, U2, U3};
    use crate::{assert_type_eq, list, set};

    #[test]
    fn graph_macro() {
        type MyGraph = graph! {};
        assert_type_eq!(<MyGraph as Size>::Out, U0);
        type WithTRex = Insert<MyGraph, (TyranosaurusRex, ())>;
        assert_type_eq!(<WithTRex as Size>::Out, U1);

        type MyDinos = graph! {
            (Compsognathus, ()): [],
            (TyranosaurusRex, ()): []
        };
        assert_type_eq!(<MyDinos as Size>::Out, U2);
    }

    #[test]
    fn remove() {
        type MyDinos = graph! {
            (Compsognathus, ()): [TyranosaurusRex],
            (TyranosaurusRex, ()): []
        };
        type WithoutCompsog = Remove<MyDinos, Compsognathus>;

        assert_type_eq!(<WithoutCompsog as Size>::Out, U1);
        assert_type_eq!(<WithoutCompsog as Source>::Out, TyranosaurusRex);

        type FoodChain = graph! {
            (TyranosaurusRex, u128): [Velociraptor, Iguanodon, Compsognathus, Pterodactyl, Oviraptor],
            (Velociraptor, u64): [Iguanodon, Compsognathus, Oviraptor, Pterodactyl],
            (Pterodactyl, usize): [Compsognathus],
            (Oviraptor, u32): [Compsognathus],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };

        type Ids = set![TyranosaurusRex, Compsognathus, Iguanodon];
        type Clamped = <(FoodChain, Ids) as RemoveNodes>::Out;

        assert_type_eq!(<Clamped as Size>::Out, U3);
        assert_type_eq!(<Clamped as Sources>::Out, set![Velociraptor]);
        assert_type_eq!(<Clamped as Sinks>::Out, set![Oviraptor, Pterodactyl]);
    }

    #[test]
    fn connection() {
        type MyDinos = graph! {
            (Compsognathus, ()): [],
            (TyranosaurusRex, ()): [],
            (Velociraptor, ()): [],
            (Iguanodon, ()): []
        };

        type Connect2 = Connect<MyDinos, Velociraptor, TyranosaurusRex>;
        type ExIn1 = set![Velociraptor];
        type ExOut1 = set![TyranosaurusRex];
        assert_type_eq!(Incoming<Connect2, TyranosaurusRex>, ExIn1);
        assert_type_eq!(Outgoing<Connect2, Velociraptor>, ExOut1);

        type Connect3 = Connect<Connect2, Iguanodon, TyranosaurusRex>;
        type ExIn2 = set![Iguanodon, Velociraptor];
        type ExOut2 = set![TyranosaurusRex];
        assert_type_eq!(Incoming<Connect3, TyranosaurusRex>, ExIn2);
        assert_type_eq!(Outgoing<Connect3, Iguanodon>, ExOut2);

        type Disco = Disconnect<Connect3, Velociraptor, TyranosaurusRex>;
        type ExIn3 = set![Iguanodon];
        type ExOut3 = set![TyranosaurusRex];
        assert_type_eq!(Incoming<Disco, TyranosaurusRex>, ExIn3);
        assert_type_eq!(Outgoing<Disco, Iguanodon>, ExOut3);
    }

    #[test]
    fn connect_graph_macro() {
        type FoodChain = graph! {
            (TyranosaurusRex, u128): [Velociraptor, Iguanodon, Compsognathus, Pterodactyl, Oviraptor],
            (Velociraptor, u64): [Iguanodon, Compsognathus, Oviraptor, Pterodactyl],
            (Pterodactyl, usize): [Compsognathus],
            (Oviraptor, u32): [Compsognathus],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };

        assert_type_eq!(Incoming<FoodChain, TyranosaurusRex>, set![]);
        assert_type_eq!(Incoming<FoodChain, Velociraptor>, set![TyranosaurusRex]);
        assert_type_eq!(Incoming<FoodChain, Pterodactyl>, set![Velociraptor, TyranosaurusRex]);
        assert_type_eq!(Incoming<FoodChain, Oviraptor>, set![Velociraptor, TyranosaurusRex]);
        assert_type_eq!(Incoming<FoodChain, Iguanodon>, set![Velociraptor, TyranosaurusRex]);
        assert_type_eq!(Incoming<FoodChain, Compsognathus>, set![Oviraptor, Pterodactyl, Velociraptor, TyranosaurusRex]);

        assert_type_eq!(Outgoing<FoodChain, TyranosaurusRex>, set![Oviraptor, Pterodactyl, Compsognathus, Iguanodon, Velociraptor]);
        assert_type_eq!(Outgoing<FoodChain, Velociraptor>, set![Pterodactyl, Oviraptor, Compsognathus, Iguanodon]);
        assert_type_eq!(Outgoing<FoodChain, Pterodactyl>, set![Compsognathus]);
        assert_type_eq!(Outgoing<FoodChain, Oviraptor>, set![Compsognathus]);
        assert_type_eq!(Outgoing<FoodChain, Iguanodon>, set![]);
        assert_type_eq!(Outgoing<FoodChain, Compsognathus>, set![]);
    }

    #[test]
    fn sources_sinks() {
        type FoodChain = graph! {
            (TyranosaurusRex, u128): [Velociraptor, Iguanodon, Compsognathus, Pterodactyl, Oviraptor],
            (Velociraptor, u64): [Iguanodon, Compsognathus, Oviraptor, Pterodactyl],
            (Pterodactyl, usize): [Compsognathus],
            (Oviraptor, u32): [Compsognathus],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };

        type Top = <FoodChain as Sources>::Out;
        assert_type_eq!(Top, set![TyranosaurusRex]);

        type Bottom = <FoodChain as Sinks>::Out;
        assert_type_eq!(Bottom, set![Iguanodon, Compsognathus]);
    }

    #[test]
    fn data() {
        type FoodChain = graph! {
            (TyranosaurusRex, u128): [Velociraptor, Iguanodon, Compsognathus, Pterodactyl, Oviraptor],
            (Velociraptor, u64): [Iguanodon, Compsognathus, Oviraptor, Pterodactyl],
            (Pterodactyl, usize): [Compsognathus],
            (Oviraptor, u32): [Compsognathus],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };

        assert_type_eq!(Get<FoodChain, TyranosaurusRex>, u128);
        assert_type_eq!(Get<FoodChain, Velociraptor>, u64);
        assert_type_eq!(Get<FoodChain, Pterodactyl>, usize);
        assert_type_eq!(Get<FoodChain, Oviraptor>, u32);
        assert_type_eq!(Get<FoodChain, Iguanodon>, u16);
        assert_type_eq!(Get<FoodChain, Compsognathus>, u8);
    }

    #[test]
    fn topological_sort() {
        type FoodChain = graph! {
            (TyranosaurusRex, u128): [Velociraptor, Iguanodon, Compsognathus, Pterodactyl, Oviraptor, Pachycephalosaurus, Stegosaurus, Triceratops],
            (Velociraptor, u64): [Iguanodon, Compsognathus, Oviraptor, Pterodactyl],
            (Pterodactyl, usize): [Compsognathus],
            (Oviraptor, u32): [Compsognathus],
            (Pachycephalosaurus, i16): [],
            (Stegosaurus, i32): [],
            (Triceratops, i64): [],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };

        assert_type_eq!(
            list![
                set![TyranosaurusRex],
                set![Triceratops, Pachycephalosaurus, Velociraptor, Stegosaurus],
                set![Oviraptor, Iguanodon, Pterodactyl],
                set![Compsognathus]
            ],
            Topo<FoodChain>
        );

        type Transposed = <FoodChain as Transpose>::Out;
        assert_type_eq!(
            list![
                set![
                    Compsognathus,
                    Iguanodon,
                    Triceratops,
                    Stegosaurus,
                    Pachycephalosaurus
                ],
                set![Oviraptor, Pterodactyl],
                set![Velociraptor],
                set![TyranosaurusRex]
            ],
            Topo<Transposed>
        );
    }
}
