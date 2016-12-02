--- Day 22: Wizard Simulator 20XX ---

Little Henry Case decides that defeating bosses with swords and stuff is
boring. Now he's playing the game with a wizard. Of course, he gets stuck on
another boss and needs your help again.

In this version, combat still proceeds with the player and the boss taking
alternating turns. The player still goes first. Now, however, you don't get
any equipment; instead, you must choose one of your spells to cast. The
first character at or below 0 hit points loses.

Since you're a wizard, you don't get to wear armor, and you can't attack
normally. However, since you do magic damage, your opponent's armor is
ignored, and so the boss effectively has zero armor as well. As before, if
armor (from a spell, in this case) would reduce damage below 1, it becomes 1
instead - that is, the boss' attacks always deal at least 1 damage.

On each of your turns, you must select one of your spells to cast. If you
cannot afford to cast any spell, you lose. Spells cost mana; you start with
500 mana, but have no maximum limit. You must have enough mana to cast a
spell, and its cost is immediately deducted when you cast it. Your spells
are Magic Missile, Drain, Shield, Poison, and Recharge.

-   Magic Missile costs 53 mana. It instantly does 4 damage.
-   Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit
    points.
-   Shield costs 113 mana. It starts an effect that lasts for 6 turns. While
    it is active, your armor is increased by 7.
-   Poison costs 173 mana. It starts an effect that lasts for 6 turns. At
    the start of each turn while it is active, it deals the boss 3 damage.
-   Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At
    the start of each turn while it is active, it gives you 101 new mana.

Effects all work the same way. Effects apply at the start of both the
player's turns and the boss' turns. Effects are created with a timer (the
number of turns they last); at the start of each turn, after they apply any
effect they have, their timer is decreased by one. If this decreases the
timer to zero, the effect ends. You cannot cast a spell that would start an
effect which is already active. However, effects can be started on the same
turn they end.

For example, suppose the player has 10 hit points and 250 mana, and that the
boss has 13 hit points and 8 damage:

  -- Player turn --
  - Player has 10 hit points, 0 armor, 250 mana
  - Boss has 13 hit points
  Player casts Poison.

  -- Boss turn --
  - Player has 10 hit points, 0 armor, 77 mana
  - Boss has 13 hit points
  Poison deals 3 damage; its timer is now 5.
  Boss attacks for 8 damage.

  -- Player turn --
  - Player has 2 hit points, 0 armor, 77 mana
  - Boss has 10 hit points
  Poison deals 3 damage; its timer is now 4.
  Player casts Magic Missile, dealing 4 damage.

  -- Boss turn --
  - Player has 2 hit points, 0 armor, 24 mana
  - Boss has 3 hit points
  Poison deals 3 damage. This kills the boss, and the player wins.

Now, suppose the same initial conditions, except that the boss has 14 hit
points instead:

  -- Player turn --
  - Player has 10 hit points, 0 armor, 250 mana
  - Boss has 14 hit points
  Player casts Recharge.

  -- Boss turn --
  - Player has 10 hit points, 0 armor, 21 mana
  - Boss has 14 hit points
  Recharge provides 101 mana; its timer is now 4.
  Boss attacks for 8 damage!

  -- Player turn --
  - Player has 2 hit points, 0 armor, 122 mana
  - Boss has 14 hit points
  Recharge provides 101 mana; its timer is now 3.
  Player casts Shield, increasing armor by 7.

  -- Boss turn --
  - Player has 2 hit points, 7 armor, 110 mana
  - Boss has 14 hit points
  Shield's timer is now 5.
  Recharge provides 101 mana; its timer is now 2.
  Boss attacks for 8 - 7 = 1 damage!

  -- Player turn --
  - Player has 1 hit point, 7 armor, 211 mana
  - Boss has 14 hit points
  Shield's timer is now 4.
  Recharge provides 101 mana; its timer is now 1.
  Player casts Drain, dealing 2 damage, and healing 2 hit points.

  -- Boss turn --
  - Player has 3 hit points, 7 armor, 239 mana
  - Boss has 12 hit points
  Shield's timer is now 3.
  Recharge provides 101 mana; its timer is now 0.
  Recharge wears off.
  Boss attacks for 8 - 7 = 1 damage!

  -- Player turn --
  - Player has 2 hit points, 7 armor, 340 mana
  - Boss has 12 hit points
  Shield's timer is now 2.
  Player casts Poison.

  -- Boss turn --
  - Player has 2 hit points, 7 armor, 167 mana
  - Boss has 12 hit points
  Shield's timer is now 1.
  Poison deals 3 damage; its timer is now 5.
  Boss attacks for 8 - 7 = 1 damage!

  -- Player turn --
  - Player has 1 hit point, 7 armor, 167 mana
  - Boss has 9 hit points
  Shield's timer is now 0.
  Shield wears off, decreasing armor by 7.
  Poison deals 3 damage; its timer is now 4.
  Player casts Magic Missile, dealing 4 damage.

  -- Boss turn --
  - Player has 1 hit point, 0 armor, 114 mana
  - Boss has 2 hit points
  Poison deals 3 damage. This kills the boss, and the player wins.

You start with 50 hit points and 500 mana points. The boss's actual stats
are in your puzzle input. What is the least amount of mana you can spend and
still win the fight? (Do not include mana recharge effects as "spending"
negative mana.)

> import Helpers
>
> start = Game 50 500 58 9 0 0 0
>
> data Spell = M | D | S | P | R | X deriving (Show, Eq)
> data Game = Game  { playerHP, mana, bossHP, bossDamage
>                   , shield, poison, recharge :: Int }
>           | Win
>           | Lose
>   deriving (Show, Eq)
>
> gfM :: [(Game -> Game)] -> Game -> Game
> gfM []      g = g
> gfM (f:fs)  g = gfM fs $ gf f g
>
> gf :: (Game -> Game) -> Game -> Game
> gf _ Win  = Win
> gf _ Lose = Lose
> gf f g    = f g
>
> useMana     i g = let x = mana g
>                    in if x <= i then Lose else g { mana = x - i }
> hitBoss     i g = let x = bossHP g
>                    in if x <= i then Win  else g { bossHP = x - i }
> hitPlayer   i g = let x = playerHP g
>                    in if x <= i then Lose else g { playerHP = x - i }
> healPlayer  i g = hitPlayer (0 - i) g
> shieldOn      g = let x = shield g
>                    in if x > 0  then Lose else g { shield = 6 }
> poisonOn      g = let x = poison g
>                    in if x > 0  then Lose else g { poison = 6 }
> rechargeOn    g = let x = recharge g
>                    in if x > 0  then Lose else g { recharge = 5 }
> tickShield g
>   | shield g == 0   = g
>   | otherwise       = g { shield = shield g - 1 }
>
> tickPoison g
>   | poison g == 0   = g
>   | otherwise       = hitBoss 3 (g { poison = poison g - 1})
>
> tickRecharge g
>   | recharge g == 0 = g
>   | otherwise       = useMana (-101) (g { recharge = recharge g - 1})
>
> tick = gfM [tickShield, tickPoison, tickRecharge]
>
> castSpell M g = gfM [ useMana  53,  hitBoss 4                 ] g
> castSpell D g = gfM [ useMana  73,  hitBoss 2,  healPlayer 2  ] g
> castSpell S g = gfM [ useMana 113,  shieldOn                  ] g
> castSpell P g = gfM [ useMana 173,  poisonOn                  ] g
> castSpell R g = gfM [ useMana 229,  rechargeOn                ] g
>
> bossHits g    = let a = if shield g > 0 then 7 else 0
>                  in hitPlayer (max 1 (bossDamage g - a)) g
>
> checkState g
>   | bossHP g    <= 0  = Win
>   | playerHP g  <= 0  = Lose
>   | otherwise         = g
>
> hardMode True   = hitPlayer 1
> hardMode False  = id
>
> playTurn s h g  = gfM [ hardMode h, castSpell s, tick
>                       , bossHits, tick, checkState ] g
>
> manaCost acc (M:xs) = manaCost (acc +  53) xs
> manaCost acc (D:xs) = manaCost (acc +  73) xs
> manaCost acc (S:xs) = manaCost (acc + 113) xs
> manaCost acc (P:xs) = manaCost (acc + 173) xs
> manaCost acc (R:xs) = manaCost (acc + 229) xs
> manaCost acc (X:xs) = 0
> manaCost acc []     = acc
>
> depthFirstSolve :: Int -> Bool -> Game -> [[Spell]]
> depthFirstSolve 0 _ _     = [[X]]
> depthFirstSolve _ _ Lose  = [[X]]
> depthFirstSolve _ _ Win   = [[ ]]
> depthFirstSolve y h x     = if null rs then [[X]] else rs
>   where
>     f s = map (s :) . depthFirstSolve (y - 1) h $ playTurn s h x
>     rs  = filter (null . filter ((== X))) . concatMap f $ [R,P,S,D,M]
>
> day22 = minimum . map (manaCost 0) . depthFirstSolve 10 False $ start


--- Part Two ---

On the next run through the game, you increase the difficulty to hard.

At the start of each player turn (before any other effects apply), you lose
1 hit point. If this brings you to or below 0 hit points, you lose.

With the same starting stats for you and the boss, what is the least amount of
mana you can spend and still win the fight?

> day22p2 = minimum . map (manaCost 0) . depthFirstSolve 10 True $ start
