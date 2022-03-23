import cats.arrow.FunctionK

object optionToList extends FunctionK[Option, List] {
  def apply[A](fa: Option[A]): List[A] = {
    fa.fold(List.empty[A])(List(_))
  }
}

optionToList(Some(1))
optionToList(None)
