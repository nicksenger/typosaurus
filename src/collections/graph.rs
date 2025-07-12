use core::marker::PhantomData;

use typenum::U0;

use crate::{
    cmp::IsEqual,
    num::Addition,
    traits::{
        fold::Foldable,
        functor::{Map, Mapper},
        semigroup::Mappend,
    },
};

use super::{
    list::{self, Filter, IntoOnes, Len, List, Ones, UntupleLeft, UntupleRight, ZipTuple},
    maybe::{self, Just, Nothing},
    record::{self, Record},
    set::{self, Contains, Set},
    tuple, Container,
};

pub struct Graph<Nodes, IncomingEdges, OutgoingEdges>(
    PhantomData<Nodes>,
    PhantomData<IncomingEdges>,
    PhantomData<OutgoingEdges>,
);

pub type Empty = Graph<record::Empty, record::Empty, record::Empty>;

pub trait Identifiers {
    type Out;
}
impl<Ids, Nodes, In, Out> Identifiers for Graph<Record<Ids, Nodes>, In, Out> {
    type Out = Ids;
}
pub type NodeIds<T> = <T as Identifiers>::Out;

pub struct WithKeys;
impl<K, V> Mapper<(K, Set<V>)> for WithKeys
where
    V: Container,
    (V, list::With<K>): Map<<V as Container>::Content, list::With<K>>,
{
    type Out = <(V, list::With<K>) as Map<<V as Container>::Content, list::With<K>>>::Out;
}

pub trait OutgoingEdgeList {
    type Out;
}
impl<N, I, Ids, Out> OutgoingEdgeList for Graph<N, I, Record<Ids, Out>>
where
    Out: Container,
    (Out, WithKeys): Map<<Out as Container>::Content, WithKeys>,
    <(Out, WithKeys) as Map<<Out as Container>::Content, WithKeys>>::Out: Foldable<list::Concat>,
{
    type Out = <<(Out, WithKeys) as Map<<Out as Container>::Content, WithKeys>>::Out as Foldable<
        list::Concat,
    >>::Out;
}
pub type Edges<G> = <G as OutgoingEdgeList>::Out;

pub trait ContainsId {
    type Out;
}
impl<T, Nodes, In, Out> ContainsId for (Graph<Nodes, In, Out>, T)
where
    (Nodes, T): record::ContainsKey,
{
    type Out = <(Nodes, T) as record::ContainsKey>::Out;
}

impl<N1, I1, O1, N2, I2, O2> Mappend for (Graph<N1, I1, O1>, Graph<N2, I2, O2>)
where
    (Graph<N1, I1, O1>, Graph<N2, I2, O2>): Merge,
{
    type Out = <(Graph<N1, I1, O1>, Graph<N2, I2, O2>) as Merge>::Out;
}

pub trait Merge {
    type Out;
}
impl<Nk1, Nv1, Nk2, Nv2, Ink1, Inv1, Ink2, Inv2, Outk1, Outv1, Outk2, Outv2> Merge
    for (
        Graph<Record<Set<Nk1>, Nv1>, Record<Set<Ink1>, Inv1>, Record<Set<Outk1>, Outv1>>,
        Graph<Record<Set<Nk2>, Nv2>, Record<Set<Ink2>, Inv2>, Record<Set<Outk2>, Outv2>>,
    )
where
    (Set<Nk1>, Set<Nk2>): set::Difference,
    (Set<Nk2>, Set<Nk1>): set::Difference,
    (Set<Nk1>, Set<Nk2>): set::Intersection,
    (
        Record<Set<Nk1>, Nv1>,
        <(Set<Nk1>, Set<Nk2>) as set::Intersection>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Nk2>, Nv2>,
        <(Set<Nk1>, Set<Nk2>) as set::Intersection>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Nk1>, Nv1>,
        <(Set<Nk1>, Set<Nk2>) as set::Difference>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Nk2>, Nv2>,
        <(Set<Nk2>, Set<Nk1>) as set::Difference>::Out,
    ): record::RemoveKeys,
    <(
        Record<Set<Nk1>, Nv1>,
        <(Set<Nk1>, Set<Nk2>) as set::Difference>::Out,
    ) as record::RemoveKeys>::Out: record::Fields,
    <(
        Record<Set<Nk1>, Nv1>,
        <(Set<Nk1>, Set<Nk2>) as set::Intersection>::Out,
    ) as record::RemoveKeys>::Out: record::Fields,
    (Set<Ink1>, Set<Ink2>): set::Difference,
    (Set<Ink2>, Set<Ink1>): set::Difference,
    (Set<Ink1>, Set<Ink2>): set::Intersection,
    (
        Record<Set<Ink1>, Inv1>,
        <(Set<Ink1>, Set<Ink2>) as set::Intersection>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Ink2>, Inv2>,
        <(Set<Ink1>, Set<Ink2>) as set::Intersection>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Ink1>, Inv1>,
        <(Set<Ink1>, Set<Ink2>) as set::Difference>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Ink2>, Inv2>,
        <(Set<Ink2>, Set<Ink1>) as set::Difference>::Out,
    ): record::RemoveKeys,
    <(
        Record<Set<Ink1>, Inv1>,
        <(Set<Ink1>, Set<Ink2>) as set::Difference>::Out,
    ) as record::RemoveKeys>::Out: record::Fields,
    <(
        Record<Set<Ink1>, Inv1>,
        <(Set<Ink1>, Set<Ink2>) as set::Intersection>::Out,
    ) as record::RemoveKeys>::Out: record::Fields,
    (Set<Outk1>, Set<Outk2>): set::Difference,
    (Set<Outk2>, Set<Outk1>): set::Difference,
    (Set<Outk1>, Set<Outk2>): set::Intersection,
    (
        Record<Set<Outk1>, Outv1>,
        <(Set<Outk1>, Set<Outk2>) as set::Intersection>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Outk2>, Outv2>,
        <(Set<Outk1>, Set<Outk2>) as set::Intersection>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Outk1>, Outv1>,
        <(Set<Outk1>, Set<Outk2>) as set::Difference>::Out,
    ): record::RemoveKeys,
    (
        Record<Set<Outk2>, Outv2>,
        <(Set<Outk2>, Set<Outk1>) as set::Difference>::Out,
    ): record::RemoveKeys,
    <(
        Record<Set<Outk1>, Outv1>,
        <(Set<Outk1>, Set<Outk2>) as set::Difference>::Out,
    ) as record::RemoveKeys>::Out: record::Fields,
    <(
        Record<Set<Outk1>, Outv1>,
        <(Set<Outk1>, Set<Outk2>) as set::Intersection>::Out,
    ) as record::RemoveKeys>::Out: record::Fields,
    (Record<Set<Nk1>, Nv1>, Record<Set<Nk2>, Nv2>): record::Merge,
    (Record<Set<Ink1>, Inv1>, Record<Set<Ink2>, Inv2>): record::Merge,
    (Record<Set<Outk1>, Outv1>, Record<Set<Outk2>, Outv2>): record::Merge,
{
    type Out = Graph<
        <(Record<Set<Nk1>, Nv1>, Record<Set<Nk2>, Nv2>) as record::Merge>::Out,
        <(Record<Set<Ink1>, Inv1>, Record<Set<Ink2>, Inv2>) as record::Merge>::Out,
        <(Record<Set<Outk1>, Outv1>, Record<Set<Outk2>, Outv2>) as record::Merge>::Out,
    >;
}
pub type Combine<A, B> = <(A, B) as Merge>::Out;

pub trait MapIds {
    type Out;
}
impl<Nodes, In, Out, M> MapIds for (Graph<Nodes, In, Out>, M)
where
    (Nodes, M): record::MapKeys,
    (In, M): record::MapKeys,
    (Out, M): record::MapKeys,
{
    type Out = Graph<
        <(Nodes, M) as record::MapKeys>::Out,
        <(In, M) as record::MapKeys>::Out,
        <(Out, M) as record::MapKeys>::Out,
    >;
}

pub trait InsertNode {
    type Out;
}
impl<Id, Data, IncomingEdges, OutgoingEdges, Nodes> InsertNode
    for (Graph<Nodes, IncomingEdges, OutgoingEdges>, (Id, Data))
where
    (Nodes, (Id, Data)): record::InsertField,
    (IncomingEdges, (Id, Set<list::Empty>)): record::InsertField,
    (OutgoingEdges, (Id, Set<list::Empty>)): record::InsertField,
{
    type Out = Graph<
        <(Nodes, (Id, Data)) as record::InsertField>::Out,
        <(IncomingEdges, (Id, Set<list::Empty>)) as record::InsertField>::Out,
        <(OutgoingEdges, (Id, Set<list::Empty>)) as record::InsertField>::Out,
    >;
}
pub type Insert<T, Node> = <(T, Node) as InsertNode>::Out;

pub trait MaybeInsertNode {
    type Out;
}
impl<Data, I, O, N> MaybeInsertNode for (Graph<N, I, O>, (Nothing, Data)) {
    type Out = Graph<N, I, O>;
}
impl<Id, Data, I, O, N> MaybeInsertNode for (Graph<N, I, O>, (Just<Id>, Data))
where
    (Graph<N, I, O>, (Id, Data)): InsertNode,
{
    type Out = <(Graph<N, I, O>, (Id, Data)) as InsertNode>::Out;
}
impl<I, O, N> MaybeInsertNode for (Graph<N, I, O>, Nothing) {
    type Out = Graph<N, I, O>;
}
impl<Id, Data, I, O, N> MaybeInsertNode for (Graph<N, I, O>, Just<(Id, Data)>)
where
    (Graph<N, I, O>, (Id, Data)): InsertNode,
{
    type Out = <(Graph<N, I, O>, (Id, Data)) as InsertNode>::Out;
}

pub trait MaybeInsertFiltered {
    type Out;
}
impl<Data, I, O, Ids, N> MaybeInsertFiltered for (Graph<Record<Ids, N>, I, O>, (Nothing, Data)) {
    type Out = Graph<Record<Ids, N>, I, O>;
}
impl<I, O, Ids, N> MaybeInsertFiltered for (Graph<Record<Ids, N>, I, O>, Nothing) {
    type Out = Graph<Record<Ids, N>, I, O>;
}
impl<T, Data, I, O, Ids, N> MaybeInsertFiltered for (Graph<Record<Ids, N>, I, O>, (Just<T>, Data))
where
    (Ids, T): Contains,
    (Just<T>, set::WithNotContainedIn<Ids>): Map<T, set::WithNotContainedIn<Ids>>,
    <(Just<T>, set::WithNotContainedIn<Ids>) as Map<T, set::WithNotContainedIn<Ids>>>::Out:
        Foldable<maybe::Filter>,
    (
        Graph<Record<Ids, N>, I, O>,
        (
            <<(Just<T>, set::WithNotContainedIn<Ids>) as Map<T, set::WithNotContainedIn<Ids>>>::Out as Foldable<maybe::Filter>>::Out,
            Data
        )
    ): MaybeInsertNode,
{
    type Out = <(
        Graph<Record<Ids, N>, I, O>,
        (
            <<(Just<T>, set::WithNotContainedIn<Ids>) as Map<T, set::WithNotContainedIn<Ids>>>::Out as Foldable<maybe::Filter>>::Out,
            Data
        )
    ) as MaybeInsertNode>::Out;
}
impl<T, Data, I, O, Ids, N> MaybeInsertFiltered for (Graph<Record<Ids, N>, I, O>, Just<(T, Data)>)
where
    (Ids, T): Contains,
    (Just<T>, set::WithNotContainedIn<Ids>): Map<T, set::WithNotContainedIn<Ids>>,
    <(Just<T>, set::WithNotContainedIn<Ids>) as Map<T, set::WithNotContainedIn<Ids>>>::Out:
        Foldable<maybe::Filter>,
        (
            Graph<Record<Ids, N>, I, O>,
            (
                <<(Just<T>, set::WithNotContainedIn<Ids>) as Map<T, set::WithNotContainedIn<Ids>>>::Out as Foldable<maybe::Filter>>::Out,
                Data
            )
        ): MaybeInsertNode,
{
    type Out = <(
        Graph<Record<Ids, N>, I, O>,
        (
            <<(Just<T>, set::WithNotContainedIn<Ids>) as Map<T, set::WithNotContainedIn<Ids>>>::Out as Foldable<maybe::Filter>>::Out,
            Data
        )
    ) as MaybeInsertNode>::Out;
}
pub type MaybeInsert<T, Node> = <(T, Node) as MaybeInsertFiltered>::Out;

pub trait MaybeInsertNodes {
    type Out;
}
impl<N, I, O> MaybeInsertNodes for (Graph<N, I, O>, list::Empty) {
    type Out = Graph<N, I, O>;
}
impl<T, U, N, I, O> MaybeInsertNodes for (Graph<N, I, O>, list::List<(T, U)>)
where
    (Graph<N, I, O>, T): MaybeInsertNode,
    (<(Graph<N, I, O>, T) as MaybeInsertNode>::Out, U): MaybeInsertNodes,
{
    type Out = <(<(Graph<N, I, O>, T) as MaybeInsertNode>::Out, U) as MaybeInsertNodes>::Out;
}
pub type MultiInsert<Graph, Nodes> = <(Graph, Nodes) as MaybeInsertNodes>::Out;

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

pub struct ExcludeIds<Ids>(PhantomData<Ids>);
impl<K, T, Ids> Mapper<(K, Set<T>)> for ExcludeIds<Ids>
where
    (Set<T>, Ids): set::Difference,
{
    type Out = (K, <(Set<T>, Ids) as set::Difference>::Out);
}
pub type Excluded<T, U> =
    <(T, ExcludeIds<U>) as Map<<T as Container>::Content, ExcludeIds<U>>>::Out;

pub trait RemoveNodes {
    type Out;
}
impl<Ids, Ids1, Ids2, In, Out, Nodes> RemoveNodes
    for (
        Graph<Nodes, Record<Set<Ids1>, In>, Record<Set<Ids2>, Out>>,
        Ids,
    )
where
    (Nodes, Ids): record::RemoveKeys,
    In: Container,
    (In, ExcludeIds<Ids>): Map<<In as Container>::Content, ExcludeIds<Ids>>,
    (Record<Set<Ids1>, Excluded<In, Ids>>, Ids): record::RemoveKeys,
    Out: Container,
    (Out, ExcludeIds<Ids>): Map<<Out as Container>::Content, ExcludeIds<Ids>>,
    (Record<Set<Ids2>, Excluded<Out, Ids>>, Ids): record::RemoveKeys,
{
    type Out = Graph<
        <(Nodes, Ids) as record::RemoveKeys>::Out,
        <(Record<Set<Ids1>, Excluded<In, Ids>>, Ids) as record::RemoveKeys>::Out,
        <(Record<Set<Ids2>, Excluded<Out, Ids>>, Ids) as record::RemoveKeys>::Out,
    >;
}

pub trait MaybeConnectNodes {
    type Out;
}
impl<IncomingEdges, OutgoingEdges, Nodes> MaybeConnectNodes
    for (
        Graph<Nodes, IncomingEdges, OutgoingEdges>,
        (Nothing, Nothing),
    )
{
    type Out = Graph<Nodes, IncomingEdges, OutgoingEdges>;
}
impl<FromId, IncomingEdges, OutgoingEdges, Nodes> MaybeConnectNodes
    for (
        Graph<Nodes, IncomingEdges, OutgoingEdges>,
        (Just<FromId>, Nothing),
    )
{
    type Out = Graph<Nodes, IncomingEdges, OutgoingEdges>;
}
impl<ToId, IncomingEdges, OutgoingEdges, Nodes> MaybeConnectNodes
    for (
        Graph<Nodes, IncomingEdges, OutgoingEdges>,
        (Nothing, Just<ToId>),
    )
{
    type Out = Graph<Nodes, IncomingEdges, OutgoingEdges>;
}
impl<FromId, ToId, IncomingEdges, OutgoingEdges, Nodes> MaybeConnectNodes
    for (
        Graph<Nodes, IncomingEdges, OutgoingEdges>,
        (Just<FromId>, Just<ToId>),
    )
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
    (Graph<Nodes, IncomingEdges, OutgoingEdges>, (FromId, ToId)): ConnectNodes,
{
    type Out = <(Graph<Nodes, IncomingEdges, OutgoingEdges>, (FromId, ToId)) as ConnectNodes>::Out;
}
pub type MaybeConnect<Graph, From, To> = <(Graph, (From, To)) as MaybeConnectNodes>::Out;

pub trait MultiConnectNodes {
    type Out;
}
impl<N, I, O> MultiConnectNodes for (Graph<N, I, O>, list::Empty) {
    type Out = Graph<N, I, O>;
}
impl<T, U, N, I, O> MultiConnectNodes for (Graph<N, I, O>, List<(T, U)>)
where
    (Graph<N, I, O>, T): MaybeConnectNodes,
    (<(Graph<N, I, O>, T) as MaybeConnectNodes>::Out, U): MultiConnectNodes,
{
    type Out = <(<(Graph<N, I, O>, T) as MaybeConnectNodes>::Out, U) as MultiConnectNodes>::Out;
}
pub type MultiConnect<Graph, Nodes> = <(Graph, Nodes) as MultiConnectNodes>::Out;

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

pub trait IdList {
    type Out;
}
impl<Is, N, I, O> IdList for Graph<Record<Set<Is>, N>, I, O> {
    type Out = Is;
}

pub trait ValueList {
    type Out;
}
impl<Is, N, I, O> ValueList for Graph<Record<Set<Is>, N>, I, O>
where
    N: Container,
    (N, tuple::Right): Map<<N as Container>::Content, tuple::Right>,
{
    type Out = list::UntupleRight<N>;
}

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
    ($graph:ty$(,)?) => {
        $graph
    };
    ($graph:ty,$id:ty:[$(,)?]) => {
        $graph
    };
    ($graph:ty,$from:ty:[$to:ty]) => {
        <($graph, ($from, $to)) as $crate::collections::graph::ConnectNodes>::Out
    };
    ($graph:ty,$id:ty:[$to:ty,$($edges:ty),*]) => {
        $crate::connect_graph!($crate::connect_graph!($graph, $id: [$to]),$id:[$($edges),*])
    };
    ($graph:ty,$id:ty:[$($edges:ty),*]$($toks:tt)*) => {
        $crate::connect_graph!($crate::connect_graph!($graph,$id:[$($edges),*])$($toks)*)
    };
}

#[macro_export]
macro_rules! maybe_connect_graph {
    ($graph:ty$(,)?) => {
        $graph
    };
    ($graph:ty,$id:ty:[$(,)?]) => {
        $graph
    };
    ($graph:ty,$from:ty:[$to:ty]) => {
        <($graph, ($from, $to)) as $crate::collections::graph::MaybeConnectNodes>::Out
    };
    ($graph:ty,$id:ty:[$to:ty,$($edges:ty),*]) => {
        $crate::maybe_connect_graph!($crate::maybe_connect_graph!($graph, $id: [$to]),$id:[$($edges),*])
    };
    ($graph:ty,$id:ty:[$($edges:ty),*]$($toks:tt)*) => {
        $crate::maybe_connect_graph!($crate::maybe_connect_graph!($graph,$id:[$($edges),*])$($toks)*)
    };
}

#[macro_export]
macro_rules! merge_graphs {
    [] => { $crate::collections::graph::Empty };
    [$graph:ty$(,)?] => { $graph };
    [$graph:ty,$($graphs:ty),+$(,)?] => {
        $crate::collections::graph::Combine<$graph, $crate::merge_graphs![$($graphs),+]>
    };
}

#[macro_export]
macro_rules! insert_nodes {
    [$graph:ty$(,)?] => { $graph };
    [$graph:ty,($id:ty,$data:ty)$(,)?] => {
        $crate::collections::graph::Insert<$graph, ($id,$data)>
    };
    [$graph:ty,($id:ty,$data:ty),$(($ids:ty,$datas:ty)),*$(,)?] => {
        $crate::insert_nodes![$crate::collections::graph::Insert<$graph, ($id,$data)>, $(($ids,$datas)),*]
    };
}

#[macro_export]
macro_rules! maybe_insert_nodes {
    [$graph:ty$(,)?] => { $graph };
    [$graph:ty,($id:ty,$data:ty)$(,)?] => {
        $crate::collections::graph::MaybeInsert<$graph, ($id,$data)>
    };
    [$graph:ty,($id:ty,$data:ty),$(($ids:ty,$datas:ty)),*$(,)?] => {
        $crate::maybe_insert_nodes![$crate::collections::graph::MaybeInsert<$graph, ($id,$data)>, $(($ids,$datas)),*]
    };
}

#[allow(unused)]
#[cfg(test)]
mod test {
    use typenum::U4;

    use super::*;
    use crate::bool::False;
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

        type Possibly = MaybeConnect<MyDinos, Just<TyranosaurusRex>, Just<Compsognathus>>;
        assert_type_eq!(Incoming<Possibly, Compsognathus>, set![TyranosaurusRex]);
        type Noop = MaybeConnect<Possibly, Nothing, Nothing>;
        assert_type_eq!(Possibly, Noop);
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
        type Chain = graph! {
            (TyranosaurusRex, u128): [Velociraptor, Iguanodon, Compsognathus, Pterodactyl, Oviraptor],
            (Velociraptor, u64): [Iguanodon, Compsognathus, Oviraptor, Pterodactyl],
            (Pterodactyl, usize): [Compsognathus],
            (Oviraptor, u32): [Compsognathus],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };
        type Moar = graph! {
            (Stegosaurus, i32): [],
            (Pachycephalosaurus, i16): []
        };
        type Moaaar = graph! {
            (Triceratops, i32): []
        };
        type FoodChain = crate::merge_graphs![Chain, Moar, Moaaar];

        assert_type_eq!(Get<FoodChain, TyranosaurusRex>, u128);
        assert_type_eq!(Get<FoodChain, Velociraptor>, u64);
        assert_type_eq!(Get<FoodChain, Pterodactyl>, usize);
        assert_type_eq!(Get<FoodChain, Oviraptor>, u32);
        assert_type_eq!(Get<FoodChain, Iguanodon>, u16);
        assert_type_eq!(Get<FoodChain, Compsognathus>, u8);
        assert_type_eq!(Get<FoodChain, Stegosaurus>, i32);
        assert_type_eq!(Get<FoodChain, Pachycephalosaurus>, i16);
        assert_type_eq!(Get<FoodChain, Triceratops>, i32);
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

    #[test]
    fn map() {
        use crate::cmp::IsEqual;

        pub struct Wrapper<T>(PhantomData<T>);
        impl<T, U> IsEqual for (Wrapper<T>, Wrapper<U>)
        where
            (T, U): IsEqual,
        {
            type Out = <(T, U) as IsEqual>::Out;
        }

        pub struct Wrap;
        impl<T> Mapper<T> for Wrap {
            type Out = Wrapper<T>;
        }

        type FoodChain = graph! {
            (Oviraptor, u32): [Compsognathus],
            (Iguanodon, u16): [],
            (Compsognathus, u8): []
        };
        type MappedChain = <(FoodChain, Wrap) as MapIds>::Out;

        assert_type_eq!(<MappedChain as Size>::Out, U3);
        assert_type_eq!(Get<MappedChain, Wrapper<Oviraptor>>, u32);
        assert_type_eq!(Get<MappedChain, Wrapper<Iguanodon>>, u16);
        assert_type_eq!(Get<MappedChain, Wrapper<Compsognathus>>, u8);
    }

    #[test]
    fn merge() {
        type DinoGraph = graph! {
            (TyranosaurusRex, ()): [Iguanodon],
            (Iguanodon, ()): []
        };
        type Moar = graph! {
            (TyranosaurusRex, ()): [Pachycephalosaurus],
            (Pachycephalosaurus, ()): []
        };
        type Moaaar = graph! {
            (TyranosaurusRex, ()): [Compsognathus],
            (Compsognathus, ()): []
        };
        type Merged = crate::merge_graphs![DinoGraph, Moar, Moaaar];
        assert_type_eq!(<Merged as Size>::Out, U4);
        assert_type_eq!(Outgoing<Merged, TyranosaurusRex>, set![Compsognathus, Pachycephalosaurus, Iguanodon]);
        assert_type_eq!(Outgoing<Merged, Iguanodon>, set![]);
        assert_type_eq!(Outgoing<Merged, Pachycephalosaurus>, set![]);
        assert_type_eq!(Outgoing<Merged, Compsognathus>, set![]);

        type A = graph! {
            (TyranosaurusRex, ()): [Iguanodon, Compsognathus],
            (Iguanodon, ()): [Compsognathus],
            (Compsognathus, ()): []
        };
        type B = graph! {
            (Iguanodon, ()): [Compsognathus],
            (Compsognathus, ()): []
        };
        type C = graph! {
            (Compsognathus, ()): []
        };

        type Abc = merge_graphs![A, B, C];
        assert_type_eq!(Outgoing<Abc, TyranosaurusRex>, set![Compsognathus, Iguanodon]);
        assert_type_eq!(Outgoing<Abc, Iguanodon>, set![Compsognathus]);
        assert_type_eq!(Outgoing<Abc, Compsognathus>, set![]);
    }

    #[test]
    fn cycles() {
        type Dinos = graph! {
            (Iguanodon, ()): [Compsognathus],
            (Compsognathus, ()): [Pterodactyl],
            (Pterodactyl, ()): [Iguanodon]
        };

        assert_type_eq!(Outgoing<Dinos, Iguanodon>, set![Compsognathus]);
        assert_type_eq!(Outgoing<Dinos, Compsognathus>, set![Pterodactyl]);
        assert_type_eq!(Outgoing<Dinos, Pterodactyl>, set![Iguanodon]);
    }

    #[test]
    fn edge_lists() {
        type Dinos = graph! {
            (Iguanodon, ()): [Compsognathus],
            (Compsognathus, ()): [Pterodactyl],
            (Pterodactyl, ()): []
        };
        type Out = <Dinos as OutgoingEdgeList>::Out;
        type L = list::Len<Out>;
        assert_type_eq!(L, U2);
        assert_type_eq!(
            Out,
            list![(Iguanodon, Compsognathus), (Compsognathus, Pterodactyl)]
        );
    }

    #[test]
    fn multiinsert() {
        type Dinos = <(
            Empty,
            list![
                Just<(TyranosaurusRex, ())>,
                Nothing,
                Just<(Velociraptor, ())>
            ],
        ) as MaybeInsertNodes>::Out;
        assert_type_eq!(Get<Dinos, TyranosaurusRex>, ());
        assert_type_eq!(Get<Dinos, Velociraptor>, ());
    }

    #[test]
    fn multiconnect() {
        type Dinos = graph! {
            (TyranosaurusRex, ()): [],
            (Velociraptor, ()): [],
            (Compsognathus, ()): []
        };
        type Connected = MultiConnect<
            Dinos,
            list![
                (Just<TyranosaurusRex>, Just<Compsognathus>),
                (Just<Compsognathus>, Nothing),
                (Just<Velociraptor>, Just<Compsognathus>)
            ],
        >;
        assert_type_eq!(
            Connected,
            graph! {
                (TyranosaurusRex, ()): [Compsognathus],
                (Velociraptor, ()): [Compsognathus],
                (Compsognathus, ()): []
            }
        );
    }

    #[test]
    fn filtered() {
        type Dinos = graph! { (TyranosaurusRex, ()): [] };
        type Noop1 = <(Dinos, (Just<TyranosaurusRex>, ())) as MaybeInsertFiltered>::Out;
        assert_type_eq!(Dinos, Noop1);
        type Noop2 = <(Dinos, (Nothing, ())) as MaybeInsertFiltered>::Out;
        assert_type_eq!(Dinos, Noop2);
        type Inserted = <(Dinos, (Just<Velociraptor>, ())) as MaybeInsertFiltered>::Out;
        assert_type_eq!(
            Inserted,
            graph! {
                (Velociraptor, ()): [],
                (TyranosaurusRex, ()): []
            }
        );
    }
}
