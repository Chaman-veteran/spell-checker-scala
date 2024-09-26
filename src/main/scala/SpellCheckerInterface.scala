import Data.WordTree.*

/**
  * Used Keyboard, the only one supported for now is QWERTY
  */
type Keyboard = Array[Array[Char]]

/**
  * Complete user's input
  *
  * @param tree
  * @param prefixe
  * @return
  */
def completeWord(tree : WordTree, prefixe : String) : List[CountedWord] =
    tree.giveSuffixe(prefixe).sortBy[Int](_.freqNInfo.frequency).take(10)
