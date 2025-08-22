patches-own [
  my-sentiment                          ;; sentiment of the agent: +1 bullish, -1 bearish
  local-field                           ;; sum of the sentiment of the 4 neighbors
  number-of-shares                      ;; stock holdings (can be <0 → short position, only for traders)
  opinion-vol                           ;; perceived volatility
  propensity-to-sentiment-contagion     ;; dynamic contagion propensity
  base-propensity-to-sentiment-contagion ;; baseline contagion propensity
  news-sensitivity                      ;; sensitivity to external news
  market-impact                         ;; order submitted in the tick (positive = buy, negative = sell)
  trader-type                           ;; "trader" or "buy-hold"

  ;; Variables specific to buy-and-hold investors
  bh-holding?                           ;; true if currently holding a position
  bh-next-action-tick                   ;; tick scheduled for the next action (buy if flat, sell if holding)
  bh-hold-until                         ;; minimum tick until which they must keep the position
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
  log-price                             ;; log of asset price
  returns                               ;; returns at each tick
  sentiment-field                       ;; aggregate sentiment of traders
  news-qualitative-meaning              ;; news arrival: +1 positive, -1 negative
  number-of-traders                     ;; count of trader agents
  number-of-bh                          ;; count of buy-and-hold agents
  volatility-indicator                  ;; proxy for volatility = |returns|
  TRbull                                ;; number of bullish traders
  TRbear                                ;; number of bearish traders
  BHbull                                ;; number of bullish buy-and-hold
  BHbear                                ;; number of bearish buy-and-hold
  returns-trader                        ;; average trader contribution to returns
  returns-bh                            ;; average buy-and-hold contribution to returns
  bh-order-size                         ;; size of orders for buy-and-hold
  meanVol                               ;; cumulative volatility average
  trader-scale                          ;; scaling factor to normalize traders’ market impact
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  reset-ticks

  ask patches [

    ;; Assign agent type based on the probability share-buy-hold
    ifelse (random-float 1 < share-buy-hold)
    [ set trader-type "buy-hold" ]
    [ set trader-type "trader" ]

    ;; Initial common state variables
    set opinion-vol sigma + random-float 0.1
    set news-sensitivity (random-float max-news-sensitivity)
    set base-propensity-to-sentiment-contagion (random-float max-base-propensity-to-sentiment-contagion)
    set propensity-to-sentiment-contagion base-propensity-to-sentiment-contagion
    set market-impact 0

    ;; Initialization for traders
    if trader-type = "trader" [
      set number-of-shares 1
      set my-sentiment 0                       ;; neutral at start, sentiment decided during go
      set pcolor gray
    ]

    ;; Initialization for buy-and-hold investors
    if trader-type = "buy-hold" [

      set bh-order-size 1

      ;; Half of buy-and-hold agents are initialized as already holding a stock
      ifelse (random-normal 0 1) > 0[
        set number-of-shares number-of-shares + bh-order-size
        set bh-holding? true
        set bh-hold-until ticks + bh-min-hold + random (bh-max-hold - bh-min-hold)
        set pcolor yellow
        set bh-next-action-tick bh-hold-until
        set my-sentiment 1                     ;; buying increases positive sentiment
      ][
        set bh-holding? false
        set bh-next-action-tick ticks + bh-min-hold + random (bh-max-hold - bh-min-hold)
        set bh-hold-until -1                   ;; placeholder until first purchase
        set pcolor green + 2
      ]
    ]
  ]

  ;; Initialize global variables
  set log-price 5
  set number-of-traders count patches with [trader-type = "trader"]
  set number-of-bh      count patches with [trader-type = "buy-hold"]
  set meanVol 0
  set returns-trader 0
  set returns-bh 0
  set sentiment-field 0

  ;; Trader scaling ensures constant market impact regardless of BH proportion
  set trader-scale 1
  ifelse count patches with [trader-type = "trader"] != 0[
    set trader-scale  count patches / count patches with [trader-type = "trader"]
  ][
    set trader-scale 1
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO – MAIN LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  news-arrival                ;; generate new information
  agent-decision              ;; agents update sentiment and orders
  market-clearing             ;; price update
  update-market-sentiment     ;; contagion rule update
  compute-volatility-indicator
  do-plot
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; News Arrival mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to news-arrival
  ;; Randomly assign qualitative meaning: positive or negative
  ifelse (random-normal 0 1) > 0
  [ set news-qualitative-meaning 1 ]
  [ set news-qualitative-meaning -1 ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent decision mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to agent-decision

  ;; Reset market impact for all agents
  ask patches [ set market-impact 0 ]

  ;; Buy-and-hold sentiment dynamics
  ask patches with [trader-type = "buy-hold"] [
    set my-sentiment 0
    if (not bh-holding?) and (ticks >= bh-next-action-tick) [
      set my-sentiment 1                  ;; buy action adds bullish sentiment
    ]
    if bh-holding? and (ticks >= bh-hold-until) [
      set my-sentiment -1                 ;; sell action adds bearish sentiment
    ]
  ]

  ;; Trader sentiment dynamics
  ask patches with [trader-type = "trader"] [
    set local-field sum [my-sentiment] of neighbors4
    ifelse ((propensity-to-sentiment-contagion * local-field) +
            (news-sensitivity * news-qualitative-meaning) +
            random-normal miu opinion-vol) > 0
      [ set my-sentiment 1 ]
      [ set my-sentiment -1 ]
  ]

  ;; Sentiment indicators
  set TRbull count patches with [my-sentiment = 1 and trader-type = "trader"]
  set TRbear count patches with [my-sentiment = -1 and trader-type = "trader"]
  set BHbull count patches with [my-sentiment = 1 and trader-type = "buy-hold"]
  set BHbear count patches with [my-sentiment = -1 and trader-type = "buy-hold"]

  ;; Aggregate sentiment field for traders
  if count patches with [trader-type = "trader"] > 0
    [ set sentiment-field mean [my-sentiment] of patches with [trader-type = "trader"] ]
  if count patches with [trader-type = "trader"] = 0
    [ set sentiment-field 0 ]

  ;; Buy-and-hold market impact
  ask patches with [trader-type = "buy-hold"] [
    if (not bh-holding?) and (ticks >= bh-next-action-tick) [
      set market-impact my-sentiment * ((TRbull * trader-scale) + BHbull)
      set number-of-shares my-sentiment * market-impact
      set bh-holding? true
      set bh-hold-until ticks + bh-min-hold + (bh-max-hold - bh-min-hold)
      set pcolor yellow
      set bh-next-action-tick bh-hold-until
    ]
    if bh-holding? and (ticks >= bh-hold-until) [
      set market-impact  my-sentiment * ((TRbear * trader-scale) + BHbear)
      set number-of-shares 0
      set bh-holding? false
      set pcolor green + 2
      set bh-next-action-tick ticks + bh-min-hold + (bh-max-hold - bh-min-hold)
    ]
  ]

  ;; Traders’ market impact considering both TR and BH sentiment
  ask patches with [trader-type = "trader"] [
    ifelse my-sentiment = 1
      [ set pcolor blue - 2
        set market-impact (my-sentiment * ((TRbull * trader-scale) + BHbull))
        set number-of-shares my-sentiment * number-of-shares + market-impact ]
      [ set pcolor red - 2
        set market-impact (my-sentiment * ((TRbear * trader-scale) + BHbear))
        set number-of-shares number-of-shares + market-impact ]
  ]

  ;; Indicators of group returns
  set returns-trader mean [market-impact] of patches with [trader-type = "trader"]
  ifelse count patches with [trader-type = "buy-hold"] != 0
    [ set returns-bh mean [market-impact] of patches with [trader-type = "buy-hold"] ]
    [ set returns-bh 0 ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Market clearing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to market-clearing
  set returns mean [market-impact] of patches / market-depth
  set log-price (log-price + returns)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update market sentiment contagion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-market-sentiment
  ask patches with [trader-type = "trader"] [
    ;; Propensity to contagion is updated depending on whether
    ;; news is confirmed/contradicted by market movements
    if (returns > 0) and (news-qualitative-meaning > 0)
      [set propensity-to-sentiment-contagion base-propensity-to-sentiment-contagion + sentiment-field]
    if (returns > 0) and (news-qualitative-meaning < 0)
      [set propensity-to-sentiment-contagion base-propensity-to-sentiment-contagion - sentiment-field]
    if (returns < 0) and (news-qualitative-meaning < 0)
      [set propensity-to-sentiment-contagion base-propensity-to-sentiment-contagion - sentiment-field]
    if (returns < 0) and (news-qualitative-meaning > 0)
      [set propensity-to-sentiment-contagion base-propensity-to-sentiment-contagion + sentiment-field]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Volatility indicator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to compute-volatility-indicator
  set volatility-indicator abs returns

  ;; Recursive average volatility update
  if ticks = 0 [
    set meanVol volatility-indicator
  ]
  if ticks > 0 [
    let a (ticks / (ticks + 1))        ;; weight of past
    let b (1 / (ticks + 1))            ;; weight of current
    set meanVol (a * meanVol) + (b * volatility-indicator)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plotting routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to do-plot
  set-current-plot "Log-price"
  set-current-plot-pen "log-price"
  plot log-price

  set-current-plot "Returns (%)"
  set-current-plot-pen "returns"
  plot (returns * 100)

  set-current-plot "Volatility (%)"
  set-current-plot-pen "volatility"
  plot (volatility-indicator * 100)
end
@#$#@#$#@
GRAPHICS-WINDOW
368
10
1021
664
-1
-1
5.0
1
10
1
1
1
0
1
1
1
-64
64
-64
64
0
0
1
ticks
30.0

BUTTON
44
37
107
70
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
108
37
171
70
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
61
113
233
146
sigma
sigma
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
61
146
233
179
miu
miu
-1
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
61
178
233
211
max-news-sensitivity
max-news-sensitivity
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
60
210
361
243
max-base-propensity-to-sentiment-contagion
max-base-propensity-to-sentiment-contagion
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
61
243
233
276
market-depth
market-depth
1
100000
50000.0
100
1
NIL
HORIZONTAL

PLOT
1021
10
1745
236
Returns (%)
time
returns
0.0
5000.0
-20.0
20.0
false
false
"" ""
PENS
"returns" 1.0 0 -16777216 true "" ""

PLOT
1021
235
1745
462
Log-price
time
Log-Price
0.0
1000.0
0.0
10.0
true
false
"" ""
PENS
"Log-price" 1.0 0 -16777216 true "" ""

PLOT
1021
462
1744
663
Volatility (%)
NIL
NIL
0.0
5000.0
0.0
20.0
false
false
"" ""
PENS
"volatility" 1.0 0 -16777216 true "" ""

BUTTON
172
37
235
70
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
56
354
228
387
share-buy-hold
share-buy-hold
0
0.4
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
56
386
228
419
bh-min-hold
bh-min-hold
1
500
100.0
1
1
NIL
HORIZONTAL

MONITOR
56
509
228
554
NIL
meanVol
17
1
11

SLIDER
56
419
228
452
bh-max-hold
bh-max-hold
0
2000
600.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 3000</exitCondition>
    <metric>meanVOl</metric>
    <metric>volatility-indicator</metric>
    <metric>returns</metric>
    <metric>log-price</metric>
    <enumeratedValueSet variable="bh-min-hold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-news-sensitivity">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="miu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-buy-hold">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-base-propensity-to-sentiment-contagion">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="market-depth">
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bh-max-hold">
      <value value="600"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
