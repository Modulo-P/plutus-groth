module Groth16Test where

import           BLS12381         (test)
import           Groth16          (Proof (..), VerificationKey (..), verify)
import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (assertEqual, testCase)

vk01 :: VerificationKey
vk01 = VerificationKey
  { nPublic = 1
  , vkAlpha = [
      884254335835973708623649726843225902515012882730292989362595689678679572079921231771317068170806996078338505579806,
      3768977299080966250688020815019979226470712404975382082543343112954422303817680439779613820354767573782934373316440,
      1
    ]
  , vkBeta = [
      [
      2801315382036359271580184551867838805765997071063520052931583554992307693827563040347593044852696020880400403800183,
      2720547088220552322101421658501053569361804801106447169991638985241299183616270020289525175877218022943839744678186
      ],
      [
      1139019368208251015845183620656622771874508938093356438887657395609394230231468860652936479057193953006987748576783,
      770284704932965497202598239706215866847979347767071358071158374227966578813392253003771174891535118568391181236317
      ],
      [
      1,
      0
      ]
    ]
  , vkGamma = [
      [
      352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160,
      3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758
      ],
      [
      1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905,
      927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582
      ],
      [
      1,
      0
      ]
    ]
  , vkDelta = [
      [
      3653817707682780481400988959837792884813139044060691250486309066732559611975172107961595508869522546031232689129662,
      1141849674709460885091515179048158257162912866718788931520718365807686518957690229971427646803968066456032635732114
      ],
      [
      2540492032915892247569545602749267035529833466265680663689342492623743591035104699497312141136821058026929128894073,
      426945487787332816931873177136288041913449245111552392246454628501530427951336241004199984406283998396003691442915
      ],
      [
      1,
      0
      ]
    ]
  , vkAlphaBeta = [
    [
    [
      2537157174758535239693887740369820268060806319913514466421451213792336528690711787806246616105877796005556197045237,
      883378670067415217798045985778103534888650261678820370794039697198204061032982518787590732837490792265896961210936
    ],
    [
      940833566247149475592932896090684843399451614601365116211428746346859022944441098839686347029312043134621249946820,
      752105595486331341406807746645401371655632697445633012750799115305427219875936412685034425161125505540557146841329
    ],
    [
      902294703114244060933220743503504820389129726504260173187711817029860908659919662455641437771424551793483524647972,
      250471782684582064614893516199777234846494055030401339738865210308536984559840241975157303374353240246073887943832
    ]
    ],
    [
    [
      1574575288642293366261504254453625953119720036728568897880245263509254818812371424838473250909880199535868328316185,
      1173301574920314051361980042948006405466067210894027378181282872111012956412191109529067748549913783368439395693674
    ],
    [
      2944216868096403493012574028955084022605365319967421529101315214044552284092817475153229507466504929697959359978453,
      3125513049273860695775929839977281060275575070445046576353167792735193670388367123732191353056001728886742365693518
    ],
    [
      1587577397770624194143398909090243026714839353667474438476576520625273192428442825134758160583667205565257436592077,
      1604652939710957270663679780930562112760366381499407288427693783493107987989044301388377966230353002914987867970499
    ]
    ]
  ]
  , vkIC = [
      [
      1543158387756368305682640577732972437006142138783149846616634502202298429897559731463273773655375040974203537466722,
      2088673663149159531424996367136503441740704070304009602411162415476128434508623983231292593704123869519078025433851,
      1
      ],
      [
      3379779854616555326463337240069717300537103481222871647422326726751131356695553710288272264490199347961045579814992,
      3148331474212877925196015078803050368886350560299501354135811263836671051022075420732464407909524101161401693824468,
      1
      ]
    ]
  }

pi01 :: Proof
pi01 = Proof
  { piA = [
      3984352275393513407707458752223381552625938981874722736014424587477519416324786265219287577738019481427124666148577,
      3934357811246215342496484974341718744307142950543561986855369767107187586719026787540296884547368402577497158247886,
      1
    ]
  , piB = [
      [
      3372051825839361802267191902926554912410385994757553252424676676340216183130863424210420817445471978026226922337833,
      1383768228561621316627203185691386271068632930515025058311040033286484194936249393773572848513907248314714565823954
      ],
      [
      1221155903482790553508449256524896113531260560029665446099564785219231367859438371011146458551163658702696619239964,
      173769527298906309724813645384929913748747002524984682791528623242175734184686578339273192650865422687981596071356
      ],
      [
      1,
      0
      ]
    ]
  , piC = [
      2760002180549978667072186956156037192766725652969953377981440820425053248045665685727157004083291821437533086232223,
      1418798878631376940395924303249345283900683995745178488269207401399355667366482245751366473765081910834806927478837,
      1
    ]
  }

testProof01 :: TestTree
testProof01 = testCase "Test proof 01 verification" $ do
  assertEqual "Verify function returns True" (verify vk01 pi01 [168932]) True

testProof02 :: TestTree
testProof02 = testCase "Test proof 02 verification should fail on invalid public input" $ do
  assertEqual "Verify function returns False" (verify vk01 pi01 [42]) False
