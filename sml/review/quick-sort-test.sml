use "test-utils.sml";
use "quick-sort.sml";

val intSort = sort (op <)

val test_0 = intSort [] = []
val test_1 = intSort [1] = [1]
val test_2 = intSort [1, 2] = [1, 2]
val test_3 = intSort [2, 1] = [1, 2]
val test_4 = intSort [5,4,3,2,1] = [1,2,3,4,5]
val test_5 = sort (fn (s1, s2) => String.size(s1) > String.size(s2) ) ["bc", "xyz", "1", "12345"] = ["12345", "xyz", "bc", "1"]

val allTestsPass = allTrue [test_0, test_1, test_2, test_3, test_4, test_5]