import HP1120
import HP2130
import Test.HUnit

main = testAll

testAll = runTestTT $ TestList tests1120

tests1120 :: [Test]
tests1120 =
  ["10" ~: "encode"         ~: [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]                                    
                            ~=? encode "aaaabccaadeeee",
   "11" ~: "encodeModified" ~: [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
                            ~=? encodeModified "aaaabccaadeeee",
   "12" ~: "decodeModified" ~: "aaaabccaadeeee"
                            ~=? decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'],
   "13" ~: "encodeDirect"   ~: [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
                            ~=? encodeDirect"aaaabccaadeeee",
   "14" ~: "dupli"          ~: [1,1,2,2,3,3]
                            ~=? dupli [1,2,3],
   "15" ~: "dupli"          ~: "aaabbbccc"
                            ~=? repli "abc" 3,
   "16" ~: "dropEvery"      ~: "abdeghk"
                            ~=? dropEvery "abcdefghik" 3,
   "17" ~: "split 3"        ~: ("abc", "defghik")
                            ~=? split "abcdefghik" 3,
   "17" ~: "split 0"        ~: ("", "abcdefghik")
                            ~=? split "abcdefghik" 0,
   "17" ~: "split -3"       ~: ("abcdefg", "hik")
                            ~=? split "abcdefghik" (-3),
   "18" ~: "slice"          ~: "cdefg"
                            ~=? slice "abcdefghik" 3 7,
   "19" ~: "rotate 3"       ~: "defghabc"
                            ~=? rotate "abcdefgh" 3,
   "19" ~: "rotate -2"      ~: "ghabcdef"
                            ~=? rotate "abcdefgh" (-2),
   "20" ~: "removeAt 1"     ~: ('b',"acd")
                            ~=? removeAt 1 "abcd",
   "20" ~: "removeAt -1"    ~: ('d',"abc")
                            ~=? removeAt (-1) "abcd",
   "00" ~: "dummy"          ~: True
                            ~=? True
                            ]

tests3140 :: [Test]
tests3140 =
  ["31" ~: "isPrime"        ~: isPrime 27
                            ~=? isPrime 7,
   "00" ~: "dummy"          ~: True
                            ~=? True
                            ]
