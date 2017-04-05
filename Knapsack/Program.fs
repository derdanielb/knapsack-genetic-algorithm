open System.Threading

module Data =
    let itemsValue = [|0 .. 9|]
    let itemsWeight = [|0.0 .. 0.5 .. 5.0|]
    let knapsackSize = 5
    let knapsackMaxWeight = 10
    let individualAmount = 10
    let generations = 10
    let tournamentSize = 10
    let calculateFitness (individual : int[]) = 
        let values = [| for i in 0 .. knapsackSize-1 -> itemsWeight.[individual.[i]]|]
        Array.sum values

open Data

        


module Initialization =
    let checkContains (x : int[]) y  =
        let res = [|false|]
        for i = 0 to x.Length-1 do
            if( x.[i] = y) then
                res.[0] <- true
        res.[0]

    let generateRandomIndividual size =
        // needs sleep since rapid execution somehow creates the same individual over and over again. Sleep seems solve it
        Thread.Sleep(100) 
        let individual = Array.create size -1
        let rnd = System.Random()
        for i = 0 to individual.Length-1 do
            while individual.[i] < 0 do
                let rand = rnd.Next Data.itemsValue.Length
                if(not(checkContains individual rand)) then
                    individual.[i] <- rand
        individual

open Initialization

module Utility =
    /// Generates a tSize x 2 randomized tournament in range of 0 .. individualAmount
    let generateTournament tournamentSize individualAmount =
        let rnd = System.Random()
        let tournament = [| for i in 0 .. tournamentSize -> 
                            [|for j in 0 .. 1 -> (rnd.Next individualAmount) |] |]
        tournament
    /// Returns the index of the winner of both individuals
    let determineWinner (duelPair : int[]) (individual1 : int[]) (individual2 : int[]) = 
        if ((calculateFitness individual1) >= (calculateFitness individual2) ) then
            duelPair.[0]
        else duelPair.[1]

open Utility
        


module GeneticSelection = 
    let individuals = [| for i in 0 .. individualAmount -> generateRandomIndividual knapsackSize |]
    let tournamentA = generateTournament tournamentSize individualAmount
    let tournamentB = generateTournament tournamentSize individualAmount
    let winnersA = tournamentA |> Array.map (fun pair -> (determineWinner pair individuals.[pair.[0]] individuals.[pair.[1]]))  
    let winnersB = tournamentB |> Array.map (fun pair -> (determineWinner pair individuals.[pair.[0]] individuals.[pair.[1]]))


            
        


[<EntryPoint>]
let main argv = 
    printfn "%A" GeneticSelection.individuals
    printfn "%A" (Utility.generateTournament tournamentSize individualAmount)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code