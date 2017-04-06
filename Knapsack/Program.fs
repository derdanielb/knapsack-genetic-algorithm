module Settings =
    let rnd = System.Random()

    // SAMPLE DATASET (Kreher and Stinson)
    // a set of 15 weights and profits for a knapsack of capacity 750, 
    // with maximum fitness of 1458.

    //let itemsValue = [| 135; 139; 149; 150; 156; 163; 173; 184; 192; 201; 210; 214; 221; 229; 240 |]
    //let itemsWeight = [| 70.0; 73.0; 77.0; 80.0; 82.0; 87.0; 90.0; 94.0; 98.0; 106.0; 110.0; 113.0; 115.0; 118.0; 120.0 |]
    //let knapsackMaxWeight = 750.0
    //let maxFitness = 1458

    // SAMPLE DATASET (small)
    //let itemsValue = [| 24; 13; 23; 15; 16|]
    //let itemsWeight = [| 12.0; 7.0; 11.0; 8.0; 9.0|]
    //let knapsackMaxWeight = 26.0

    
    // random value and weight generation
    let itemsValue = Array.init 100 (fun _ -> (rnd.Next 20))
    let itemsWeight = Array.init 100 (fun _ -> (float (rnd.Next 10)))
    let knapsackMaxWeight = float (int ( ((Array.sum itemsWeight)/2.0)*0.93))
    let maxFitness = 0
    printfn "MaxWeight: %A, targetFitness: %A" knapsackMaxWeight maxFitness 
    //let itemsValue = [|0 .. 19|]
    //let itemsWeight = Array.create itemsValue.Length 1.0 |> Array.mapi (fun i value -> (float (i* ( rnd.Next 3))))
    let knapsackSize = itemsValue.Length
    let individualAmount = 10
    let generations = 10000
    let tournamentSize = 10
    let crossoverProbability = 0.4
    let mutationProbability = 0.4

    let calculateFitness (individual : int[]) = 
        let values = [| for i in 0 .. knapsackSize-1 -> if(not (individual.[i] < 0)) then itemsValue.[individual.[i]] else 0|]
        Array.sum values
    let calculateWeight (individual : int[]) = 
        let weight = [| for i in 0 .. individual.Length-1 -> if(not (individual.[i] < 0)) then itemsWeight.[individual.[i]] else 0.0|]
        Array.sum weight
    

open Settings


module Utility =
    /// Checks if array contains a specific value
    let checkContains (x : int[]) y  =
        let res = [|false|]
        for i = 0 to x.Length-1 do
            if( x.[i] = y) then
                res.[0] <- true
        res.[0]

    /// Generates a randomized tournament array of size "individualAmount" with index value pair (duel pairs) in range of 0 to "individualAmount"
    let generateTournament =
        let tournament = [| for i in 0 .. tournamentSize -> 
                            [|for j in 0 .. 1 -> (rnd.Next individualAmount) |] |]
        tournament

    /// Returns the index of the winner of both individuals
    let determineWinner (duelPair : int[]) (individual1 : int[]) (individual2 : int[]) = 
        if ((calculateFitness individual1) >= (calculateFitness individual2) ) then
            duelPair.[0]
        else 
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
                        //printfn "old: %A, new %A" (calculateWeight newIndividual) (calculateWeight tempIndividual)
                        newIndividual.[intersectionPoint+i] <- individual2.[i]
        newIndividual

    /// Returns a mutated individual if the mutation did not exceed the maximal Weight constraint.
    /// Returns the original individual if no mutation was possible
    let applyMutation (individual: int[]) =
        let mutationPoint = rnd.Next individual.Length
        let temp = Array.copy individual
        temp.[mutationPoint] <- -1
        let keepMutating = [| true |]
        for i = 0 to itemsValue.Length do
            if(keepMutating.[0]) then
                let rand = rnd.Next itemsValue.Length
                let backVal = temp.[mutationPoint]
                temp.[mutationPoint] <- rand
                if(not(checkContains individual rand) && (calculateWeight temp <= knapsackMaxWeight)) then
                    individual.[mutationPoint] <- rand
                    keepMutating.[0] <- false
                else
                    temp.[mutationPoint] <- backVal
        individual

module Initialization =

    /// Generates an individual sized 'knapsackSize' fields with a legal random knapsack package with respect to the maximum allowed weight
    let generateRandomIndividual () =
        let individual = Array.create knapsackSize -(itemsValue.Length/2)
        for i = 0 to knapsackSize-1 do
            while individual.[i] < -1 do
                let rand = rnd.Next itemsValue.Length
                if(not(Utility.checkContains individual rand)) then
                    let temp = Array.copy individual
                    temp.[i] <- rand
                    if(calculateWeight temp <= knapsackMaxWeight) then
                        individual.[i] <- rand
                else 
                    individual.[i] <- (individual.[i]+1)
        let sorted  = individual 
                        |> Array.sortDescending 
                        |> Array.map (fun index -> (if index < 0 then -1 else index))
        sorted
        

[<EntryPoint>]
let geneticSelection argv = 

    let individuals = Array.init individualAmount (fun _ ->  (Initialization.generateRandomIndividual ()))
    let mutable gen = 0
    while(gen <= generations) do

        let fitness = individuals |> Array.map (fun ind -> calculateFitness ind)
        // Save elite individual
        let eliteIndividual = Array.copy individuals.[Array.findIndex (fun item -> item = (Array.max fitness)) fitness]
        // Create tournaments and determine the winners
        let tournamentA = Utility.generateTournament 
        let tournamentB = Utility.generateTournament 
        let winnersA = tournamentA |> Array.map (fun pair -> (Utility.determineWinner pair individuals.[pair.[0]] individuals.[pair.[1]]))
        let winnersB = tournamentB |> Array.map (fun pair -> (Utility.determineWinner pair individuals.[pair.[0]] individuals.[pair.[1]]))
        // Determine if an individual will receive a crossover operation
        let crossoverEligible = Array.init individualAmount (fun _ -> ((rnd.Next 100)<= (int) (crossoverProbability*100.0)))
        // Apply crossover
        let crossoverIndividuals = crossoverEligible 
                                    |> Array.mapi (fun i crossover  -> if(crossover) then 
                                                                         (Utility.crossoverTwoIndividuals individuals.[winnersA.[i]] individuals.[winnersB.[i]]) 
                                                                        else  individuals.[winnersA.[i]])                                                                  
        // Cetermine if an individual will receive a mutation
        let mutationEligible = Array.init individualAmount (fun _ -> ((rnd.Next 100)<= (int) (mutationProbability*100.0)))
        // Apply mutation
        let mutationIndividuals = mutationEligible 
                                    |> Array.mapi (fun i mutation  -> if(mutation) then 
                                                                         (Utility.applyMutation crossoverIndividuals.[i]) 
                                                                            else crossoverIndividuals.[i])  
        // Copy individuals to preserve them for next generation
        for i = 0 to individuals.Length-1 do
            individuals.[i] <- mutationIndividuals.[i]
        // Elitism: Copy elite individual to first slot for the next generation
        individuals.[0] <- eliteIndividual
        if((gen % (generations/100)) = 0 || gen = 0 || (calculateFitness eliteIndividual = maxFitness)) then
            printfn "Generation: %d, MaxFitness: %A with weight: %A" gen (calculateFitness eliteIndividual) (calculateWeight eliteIndividual)
            if ( (calculateFitness eliteIndividual = maxFitness)) then
                gen <- generations
        gen <- gen+1
    System.Console.ReadKey() |> ignore
    0
