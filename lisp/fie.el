
(defun fie-insert-insult ()
  "Insert an Elizabethan insult at point.
The variable `fie-words' provides raw data; edit it to taste.
For maximal convenience, why not bind this function to your favorite
key sequence?  I use `C-c i' myself."
  (interactive)
  (insert "Thou " (fie-insult) "!  "))

(defun fie-insult ()
  (mapconcat 'fie-pick-random fie-words " "))

(defun fie-pick-random (words)
  (nth (random (length words)) words))

(defconst fie-words 
  '(("artless" "bawdy" "beslubbering" "bootless" "churlish" "cockered"
     "clouted" "craven" "currish" "dankish" "dissembling" "droning"
     "errant" "fawning" "fobbing" "froward" "frothy" "gleeking" "goatish"
     "gorbellied" "impertinent" "infectious" "jarring" "loggerheaded"
     "lumpish" "mammering" "mangled" "mewling" "paunchy" "pribbling"
     "puking" "puny" "quailing" "rank" "reeky" "roguish" "ruttish" "saucy"
     "spleeny" "spongy" "surly" "tottering" "unmuzzled" "vain" "venomed"
     "villainous" "warped" "wayward" "weedy" "yeasty")
    ("base-court" "bat-fowling" "beef-witted" "beetle-headed"
     "boil-brained" "clapper-clawed" "clay-brained" "common-kissing"
     "crook-pated" "dismal-dreaming" "dizzy-eyed" "doghearted"
     "dread-bolted" "earth-vexing" "elf-skinned" "fat-kidneyed"
     "fen-sucked" "flap-mouthed" "fly-bitten" "folly-fallen" "fool-born"
     "full-gorged" "guts-griping" "half-faced" "hasty-witted" "hedge-born"
     "hell-hated" "idle-headed" "ill-breeding" "ill-nurtured"
     "knotty-pated" "milk-livered" "motley-minded" "onion-eyed"
     "plume-plucked" "pottle-deep" "pox-marked" "reeling-ripe" "rough-hewn"
     "rude-growing" "rump-fed" "shard-borne" "sheep-biting" "spur-galled"
     "swag-bellied" "tardy-gaited" "tickle-brained" "toad-spotted"
     "urchin-snouted" "weather-bitten")
    ("apple-john" "baggage" "barnacle" "bladder" "boar-pig" "bugbear"
     "bum-bailey" "canker-blossom" "clack-dish" "clotpole" "coxcomb"
     "codpiece" "death-token" "dewberry" "flap-dragon" "flax-wench"
     "flirt-gill" "foot-licker" "fustilarian" "giglet" "gudgeon" "haggard"
     "harpy" "hedge-pig" "horn-beast" "hugger-mugger" "jolthead" "lewdster"
     "lout" "maggot-pie" "malt-worm" "mammet" "measle" "minnow" "miscreant"
     "moldwarp" "mumble-news" "nut-hook" "pigeon-egg" "pignut" "puttock"
     "pumpion" "ratsbane" "scut" "skainsmate" "strumpet" "varlet" "vassal"
     "whey-face" "wagtail")))


