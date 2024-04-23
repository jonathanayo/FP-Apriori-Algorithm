(*  Jonathan Ayotte, Apriori Algorithm using Functional Programming *)
structure Apriori :> APRIORI =
struct


type ItemsDict = int Dicts.ItemMap.map
type PairsDict = int Dicts.PairMap.map
type FirstPassResult = {nrecords:int, minSupport: int, popItems: ItemsDict}


fun do_first_pass (threshold: real, lines: TextIO.instream, delim: char): FirstPassResult =
(* First Pass makes dictionary of all popular items over the minimum support threshold (min number of frequency of items or greater or else it does not get added*)
    let
        val itemsDict = Dicts.ItemMap.empty

        (* Function to read lines, count total transactions and update dictionary with all frequencies of all items *)
        fun readLines (dict: ItemsDict, acc: int) =
            case TextIO.inputLine lines of
                SOME line =>
                    let
                        val lineLength = size line
                        val trimmedLine = if String.sub(line, lineLength - 1) = #"\n" then (* Remove newline character *)
                                             String.substring(line, 0, lineLength - 1)
                                         else
                                             line
                        val lineofitems = String.tokens (fn c => c = delim) trimmedLine  (* Split line into words *)
                        
                        val updatedDict = List.foldl (fn (item, d) =>  (* Update or insert item counts in the dictionary *)
                            case Dicts.ItemMap.find(d, item) of
                                NONE => Dicts.ItemMap.insert(d, item, 1)  (* If item not found, add it with count 1 *)
                              | SOME count => Dicts.ItemMap.insert(d, item, count + 1)  (* If found, increment count *)
                            ) dict lineofitems
                    in
                        readLines (updatedDict, acc + 1)  (* Add transactions to accumulator and continue reading lines to count number of transactions and update next line of frequencies for that transaction*)
                    end
              | NONE => (dict, acc)  (* If no more lines, return the accumulator and finished dictionary*)

        val (finalItemsDict, recCount) = readLines(itemsDict, 0)  (* Start reading lines and get the final count and populate dictionary with all item frequencies *)
        val minsupport = Real.floor (threshold * Real.fromInt recCount)  (* Set the minimum support threshold *)

        (* Filter out non-popular items in the created dixtiony  *)
        val popularItems = Dicts.ItemMap.filteri (fn (_, count) => count >= minsupport) finalItemsDict
    in
        {nrecords = recCount, minSupport = minsupport, popItems = popularItems }
    end




fun do_second_pass(support: int, popItems: ItemsDict, lines: TextIO.instream, delim:char): PairsDict  =
(* create PairsDict, containing all popular pairs of items over the support threshold*)
    let 
        val pairsDict = Dicts.PairMap.empty

        (* Function to read each transaction from the input stream and populate dictoomay with all pairs frequiencies*)
        fun readTransactions dict =
            case TextIO.inputLine lines of
                SOME line =>
                    let
                        val lineLength = size line
                        val trimmedLine = if String.sub(line, lineLength - 1) = #"\n" then (* Remove newline character *)
                                             String.substring(line, 0, lineLength - 1)
                                         else
                                             line
                        val lineOfItems = String.tokens (fn c => c = delim) trimmedLine  (* Split line into items *)

                        (* Filter items in the transaction that are not in the popItems dictonary *)
                        val frequentItems = List.filter (fn item => Dicts.ItemMap.inDomain(popItems, item)) lineOfItems

                        (* Generate all unique pairs of frequent items *)
                        fun generatePairs [] = []
                          | generatePairs [_] = []
                          | generatePairs items =
                                let
                                    fun generatePairs' [] _ = []
                                      | generatePairs' (x :: xs) acc =
                                            let
                                                val pairs = List.map (fn item => if x < item then (x, item) else (item, x)) acc (* Generate pairs of items, but count item1,item2 as the same as item2,item1 aka both have same key *)
                                            in
                                                pairs @ generatePairs' xs (x :: acc)
                                            end
                                in
                                    generatePairs' items []
                                end

                        val pairsOfFrequentItems = generatePairs frequentItems

                        (* Update pair frequecies in the dictionary *)
                        fun updatePairFreqs [] dict = dict
                          | updatePairFreqs ((item1, item2) :: rest) dict =
                                case Dicts.PairMap.find(dict, (item1, item2)) of
                                    NONE => updatePairFreqs rest (Dicts.PairMap.insert(dict, (item1, item2), 1))
                                  | SOME count => updatePairFreqs rest (Dicts.PairMap.insert(dict, (item1, item2), count + 1))

                        val updatedPairsDict = updatePairFreqs pairsOfFrequentItems dict
                    in
                        (* Process each transaction *)
                        readTransactions updatedPairsDict
                    end
              | NONE => dict  (* End of input stream, return the accumulated dictionary *)

        val finalPairsDict = readTransactions pairsDict
        val minSupportThreshold = support

        (* Filter the pairs dictionary to retain only pairs with freqencies above the minimum support threshold *)
        val popularPairs = Dicts.PairMap.filteri (fn (_, count) => count >= minSupportThreshold) finalPairsDict
    in
        popularPairs
    end

fun print_table(nTransactions: int, popItems: ItemsDict, popPairs: PairsDict, toPrint:int):int =
            (* For now just prints dictionaires from first and second pass, can add more funtionality to get even more stats on items etc. in future *)
    let
        val _ =  print("Popular Items Dictionary:\n")
        val _ = Dicts.ItemMap.appi (fn (k,v) => print("[" ^ k ^ "] => " ^ Int.toString v ^ "\n")) popItems
        val _ = print("\n\n")
        val _ = print("Popular Pairs Dictionary:\n")
        val _ = Dicts.PairMap.appi (fn ((item1, item2), count) =>
            print ("[" ^ item1 ^ "," ^ item2 ^ "] => " ^ Int.toString count ^ "\n")
        ) popPairs
    in 
        0
    end




end
