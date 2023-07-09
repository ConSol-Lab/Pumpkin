pub trait SequenceGeneratorInterface {
    fn next(&mut self) -> i64;
}
