use "conversion.sml";
use "test-utils.sml";

(* Test tree to list conversion *)

val minIntTree = Leaf (1);
val medIntTree = Parent (Leaf(1), 2, Leaf(3));
val slopingIntTree = 
    Parent (
        Parent (
            Parent (
                Leaf 1,
                2,
                Leaf 3
            ),
            4,
            Leaf 5
        ),
        6,
        Empty
    );

val stringTree = 
    Parent (Parent (Parent(Leaf "a", "b", Leaf "c"),
                    "d",
                    Parent (Leaf "ab", "bc", Leaf "cd" )
                    ),
            "efg",
            Parent (Parent (Leaf "a", "b", Leaf "c"), 
                            "z",
                    Parent (Leaf "x", "y", Leaf "z" 
            )
        )
    );
val stringTreeList = ["a", "b", "c", "d", "ab", "bc", "cd", "efg", "a", "b", "c", "z", "x", "y", "z"];

val treeToListTest_0 = treeToList(minIntTree) = [1];
val treeToListTest_1 = treeToList(medIntTree) = [1,2,3];
val treeToListTest_2 = treeToList(slopingIntTree) = [1,2,3,4,5,6];
val treeToListTest_3 = treeToList(stringTree) = stringTreeList;
val treeToListTest_4 = treeToList(Empty) = [];

val treeToListResults = [
        treeToListTest_0,
        treeToListTest_1,
        treeToListTest_2,
        treeToListTest_3,
        treeToListTest_4
    ];

val ttlPassed = allTrue treeToListResults;

(* Test list to tree conversion *)

val intListToTree = listToTree (op <);
val listToTreeTest_0 = intListToTree [] = Empty;
val listToTreeTest_1 = intListToTree [1] = Leaf 1;
val listToTreeTest_2 = intListToTree [1,2] = Parent(Empty, 1, Leaf 2)
val listToTreeTest_3 = intListToTree [2,1,3] = Parent(Leaf 1, 2, Leaf 3)
val listToTreeTest_4 = intListToTree [5,8,3,5,7,9] = 
Parent 
    (
    Parent 
        (
        Empty,
        3,
        Leaf 5
        )
    ,
    5,
    Parent 
        (
        Leaf 7,
        8,
        Leaf 9
        )
    );

val listToTreeResults = [
        listToTreeTest_0,
        listToTreeTest_1,
        listToTreeTest_2,
        listToTreeTest_3,   
        listToTreeTest_4
    ];

val lttPassed = allTrue listToTreeResults;

val allTestsPass = allTrue [ttlPassed, lttPassed]