#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

robot (name,attack,hp) = \message -> message (name,attack,hp)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot n = aRobot (\(_,a,h) -> robot (n,a,h))
setAttack aRobot a = aRobot (\(n,_,h) -> robot (n,a,h))
setHP aRobot h = aRobot (\(n,a,_) -> robot (n,a,h))

printInfo aRobot = aRobot (\(n,a,h) ->
  n ++ " | Attack: " ++ show a ++ " | HP: " ++ show h)

killerRobot = robot ("Stanley", 9001, 9001)
nicerRobot = setName killerRobot "Kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

damage aRobot dmg = aRobot (\(n,a,h) -> robot (n,a,h - dmg))

afterHit = damage killerRobot 90

fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
      then getAttack aRobot
      else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke", 20, 30)

fastRobotRound1 = fight slowRobot fastRobot
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2

slowRobotRound1' = fight fastRobot slowRobot
fastRobotRound1' = fight slowRobotRound1' fastRobot
slowRobotRound2' = fight fastRobotRound1' slowRobotRound1'
fastRobotRound2' = fight slowRobotRound2' fastRobotRound1'
slowRobotRound3' = fight fastRobotRound2' slowRobotRound2'
fastRobotRound3' = fight slowRobotRound3' fastRobotRound2'

getHPs robots = map getHP robots

threeRoundFight robot1 robot2 =
  (\robot2Round1 ->
    (\robot1Round1 ->
      (\robot2Round2 ->
        (\robot1Round2 ->
          (\robot2Round3 ->
            (\robot1Round3 ->
              fight robot1Round3 robot2Round3)
            fight robot2Round3 robot1Round2)
          fight robot1Round2 robot2Round2)
        fight robot2Round2 robot1Round1)
      fight robot1Round1 robot2Round1)
    fight robot2Round1 robot1)
  (fight robot1 robot2)

theFreeBots = [killerRobot, gentleGiant, slowRobot]

fightManyClosure attackerRobot = (\aRobot -> fight attackerRobot aRobot)

fightMany attackerRobot robotsToAttack =
  map getHP $ map (fightManyClosure attackerRobot) robotsToAttack
