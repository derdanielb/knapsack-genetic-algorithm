open System.Threading

module Settings =
    let itemsValue = [|0 .. 9|]
    let itemsWeight = [|0.0 .. 1.5 .. 13.5|]
    let knapsackSize = 5
    let knapsackMaxWeight = 20.0
    let individualAmount = 10
    let generations = 10
    let tournamentSize = 10
    let crossoverProbability = 0.4
    let mutationProbability = 0.4

    let calculateFitness (individual : int[]) = 
        let values = [| for i in 0 .. knapsackSize-1 -> if(not (individual.[i] < 0)) then itemsValue.[individual.[i]] else 0|]
        Array.sum values
    let calculateWeight (individual : int[]) = 
        let weight = [| for i in 0 .. individual.Length-1 -> if(not (individual.[i] < 0)) then itemsWeight.[individual.[i]] else 0.0|]
        Array.sum weight
    let rnd = System.Random()

open Settings

        



         



module Utility =

    let checkContains (x : int[]) y  =
        let res = [|false|]
        for i = 0 to x.Length-1 do
            if( x.[i] = y) then
                res.[0] <- true
        res.[0]

    /// Generates a randomized tournament array of size "individualAmount" with index value pair (duel pairs) in range of 0 to "individualAmount"
    let generateTournament =
        //let rnd = System.Random()
        let tournament = [| for i in 0 .. tournamentSize -> 
                            [|for j in 0 .. 1 -> (rnd.Next individualAmount) |] |]
        tournament

    /// Returns the index of the winner of both individuals
    let determineWinner (duelPair : int[]) (individual1 : int[]) (individual2 : int[]) = 
        if ((calculateFitness individual1) >= (calculateFitness individual2) ) then
            printfn "%A > %A" (calculateFitness individual1) (calculateFitness individual2)
            duelPair.[0]
        else 
            printfn "%A < %A" (calculateFitness individual1) (calculateFitness individual2)
            duelPair.[1]

    /// Returns a new individual as result from crossing over two individuals
    let crossoverTwoIndividuals (individual1: int[]) (individual2: int[]) =
        //let rnd = System.Random()
        let intersectionPoint = rnd.Next knapsackSize
        let newIndividual = Array.append individual1.[0..intersectionPoint] (Array.create (knapsackSize-intersectionPoint) -1)
        for i = 0 to individual2.Length-1-intersectionPoint do
            for j = 0 to individual2.Length-1 do
                if newIndividual.[i] >= 0 then             
                    let tempIndividual = Array.copy newIndividual
                    tempIndividual.[intersectionPoint+i] <-  individual2.[i]
                    // add a value from second individual if newIndividual does not contain element already
                    // and newIndividual still fulfills the weight constraint after adding
                    if(not(checkContains newIndividual individual2.[i]) &&  (calculateWeight tempIndividual <= knapsackMaxWeight)) then
                        newIndividual.[intersectionPoint+i] <- individual2.[i]
        newIndividual


module Initialization =
    let generateRandomIndividual () =
        let individual = Array.create knapsackSize -1
        for i = 0 to knapsackSize-1 do
            while individual.[i] = -1 do
                let rand = rnd.Next itemsValue.Length
                if(not(Utility.checkContains individual rand)) then
                    if(calculateWeight individual <= knapsackMaxWeight) then
                        individual.[i] <- rand
                    else 
                        individual.[i] <- -2
        let sorted  = individual |> Array.sortDescending |> Array.map (fun index -> (if index < 0 then -1 else index))
        sorted
    let test i j =
        printfn "%A %A" i j
        i
        

[<EntryPoint>]
let geneticSelection argv = 
    //let preIndividuals = Array.create individualAmount [||] 
    //let individuals = preIndividuals |> Array.map (fun content ->  (Initialization.generateRandomIndividual content))
    let individuals = Array.init individualAmount (fun _ ->  (Initialization.generateRandomIndividual ()))

    printfn "%A" individuals
    let tournamentA = Utility.generateTournament 
    let tournamentB = Utility.generateTournament 
    let winnersA = tournamentA |> Array.map (fun pair -> (Utility.determineWinner pair individuals.[pair.[0]] individuals.[pair.[1]]))
    let winnersB = tournamentB |> Array.map (fun pair -> (Utility.determineWinner pair individuals.[pair.[0]] individuals.[pair.[1]]))
    let crossoverEligible = Array.init individualAmount (fun _ -> ((rnd.Next 100)<= (int) (crossoverProbability*100.0)))
    printfn "%A" crossoverEligible
    //let newIndividuals = crossoverEligible |> Array.iteri (fun crossover i -> (Initialization.test crossover i)) 
    //printfn "%A" newIndividuals
    System.Console.ReadKey() |> ignore
    0
