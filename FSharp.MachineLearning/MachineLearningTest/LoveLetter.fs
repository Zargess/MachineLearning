namespace MachineLearningTestCases

open Zargess.MachineLearning.ReinforcementLearning
open System

module LoveLetter =
    type Card = int
    type World = {
       hand: Card list;
       opponentHand: Card list;
       discardedCards: Card list;
       deck: Card list;
    }

    let fullDeck = [ 1; 1; 1; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 7; 8]

    let shuffleDeck xs = xs |> Seq.sortBy (fun _ -> Guid.NewGuid()) |> List.ofSeq

    let getHandCardsAndNewDeck (deck : Card list) =
        let hand = [ List.head deck ]
        let opponent = [ deck |> List.tail |> List.head ]
        let newDeck = deck |> List.tail |> List.tail
        hand, opponent, newDeck

    let createInitialWorld() =
        let shuffledDeck = shuffleDeck fullDeck |> List.tail
        let hand, opponentHand, deck = getHandCardsAndNewDeck shuffledDeck
        { hand = hand; opponentHand = opponentHand; discardedCards = []; deck = deck; }

    let createInitialState() = {
        world = createInitialWorld();
        reward = 0.0;
        agent = { id = "computer" }
    }