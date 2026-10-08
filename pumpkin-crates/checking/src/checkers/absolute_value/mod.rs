mod conflict;

#[derive(Clone, Debug)]
pub struct AbsoluteValueChecker<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
}
