:- module(problems, [problem/2]).

:- use_module(problemsPosterEvaluation, [problem/2 as problemPosterEvaluation]).

problem(X, Y) :-
    problemPosterEvaluation(X, Y).

problem(p1, problem(4,5, [
                        "The perens pig lives in Slovakia and was recognized as endangered in 2009",
                        "The eldar elk has a population size of 210",
                        "The byengo bat doesn't live in Ghana",
                        "The animal that lives in Slovakia was listed sometime before the animal from Russia",
                        "Of the species with a population size of 490 and the eldar elk, one lives in Poland and the other was recognized as endangered in 2009",
                        "Neither the nibner newt nor the byengo bat has a surviving population size of 525",
                        "The species that lives in Ghana was listed 2 years after the nibner newt",
                        "The animal that lives in Russia doesn't have a surviving population size of 315"
        ], [
                        noun([species], [species]),
                        noun([animal], [animals]),
                        noun([year], [years]),
                        pn([the, perens, pig]),
                        pn([the, byengo, bat]),
                        pn([the, nibner, newt]),
                        pn([the, eldar, elk]),
                        pn([the, osbele, oryx]),
                        pn([ghana]),
                        pn([honduras]),
                        pn([poland]),
                        pn([russia]),
                        pn([slovakia]),
                        tvPrep([lives], [in], [live], [lived]),
                        tvPrep([recognized, as, endangered], [in], [recognize, as, endangered], [recognized, as, endangered]),
                        tvPrep([listed], [in], [list], [listed]),
                        tvPrep([has, a, population, size], [of], [have, a, population, size], [had, a, population, size]),
                        tvPrep([has, a, surviving, population, size], [of], [have, a, surviving, population, size], [had, a, surviving, population, size]),
                        prep([from]),
                        prep([with, a, population, size, of])
        ])).

problem(p2, problem(4,5, [
                        "Of the contestant who scored 41 points and the person who threw the white darts, one was from Worthington and the other was Ira",
                        "Bill was from Mount union",
                        "Ira scored 21 points higher than the contestant from Worthington",
                        "Oscar scored somewhat higher than the player who threw the orange darts",
                        "The contestant from Mount union threw the black darts",
                        "Pedro didn't finish with 55 points",
                        "The player who threw the red darts was either Colin or the contestant who scored 48 points",
                        "Of the contestant who scored 41 points and the person who threw the orange darts, one was from Gillbertville and the other was from Worthington",
                        "Ira scored 7 points lower than the player from Lohrville"
        ], [
                        noun([contestant], [contestants]),
                        noun([person], [persons]),
                        noun([player], [players]),
                        noun([point], [points]),
                        pn([bill]),
                        pn([colin]),
                        pn([ira]),
                        pn([oscar]),
                        pn([pedro]),
                        pn([mount, union]),
                        pn([gillbertville]),
                        pn([lohrville]),
                        pn([worthington]),
                        pn([yorktown]),
                        ppn([the, black, darts]),
                        ppn([the, orange, darts]),
                        ppn([the, red, darts]),
                        ppn([the, white, darts]),
                        ppn([the, yellow, darts]),
                        tv([threw], [throw]),
                        tv([scored], [score]),
                        tvPrep([finishes], [with], [finish], [finished]),
                        prep([from])
        ])).

% ADAPTED!!!
% added "shirts" to clue 3
problem(p3, problem(4, 5, [
                        "The team that finished with 739 points wore orange shirts",
                        "Of the squad in the blue shirts and the team from Stacyville, one had a final score of 715 points and the other was the Oddballs",
                        "Of the Turkey Rolls and the team from Lathrop, one wore orange shirts and the other wore red shirts",
                        "The rowdy rollers finished 12 points below the team in orange shirts",
                        "The squad that finished with 727 points was either the team in the orange shirts or the squad from Woodstock",
                        "The Turkey Rolls weren't from Castro Valley",
                        "The Alley Cats had a final score of 763 points",
                        "The team in the blue shirts, the Splitters, the squad from Lathrop and the Rowdy Rollers were all different teams",
                        "The squad that finished with 715 points didn't wear lime green shirts"
                    ], [
                        noun([team], [teams]),
                        noun([point], [points]),
                        noun([squad], [squads]),
                        pn([orange, shirts]),
                        pn([blue, shirts]),
                        pn([stacyville]),
                        pn([lathrop]),
                        pn([red, shirts]),
                        pn([woodstock]),
                        pn([castro, valley]),
                        pn([lime, green, shirts]),
                        ppn([oddballs]),
                        ppn([turkey, rolls]),
                        ppn([rowdy, rollers]),
                        ppn([alley, cats]),
                        ppn([splitters]),
                        tv([wore], [wear]),
                        tvPrep([finished], [with], [finish], [finished]),
                        tvPrep([had, a, final, score], [of], [have, a, final, score, of], [had, a, final, score, of]),
                        prep([in]),
                        prep([from])
                    ])).

%ADAPTED
% isn't playing -> doesn't play (clue 6)
% dropped now from clue 7
% adapted clue 8 to be a real enumeration
% dropped "currently" from clue 10
problem(p4, problem(4, 5, [
                        "Michael isn't from Eldon",
                        "The 2008 graduate is either Jackie or Michael",
                        "The 2006 graduate is either Ivan or Orlando",
                        "The Dodgers player didn't graduate in 2007 or 2008",
                        "Jackie graduated 2 years before the player from York",
                        "The 2007 graduate doesn't play for the Giants",
                        "Of Lonnie and the person from Frenchboro, one is with the Dodgers and the other graduated in 2005",
                        %% "The five players are the person from Frenchboro, Ivan, and the three players currently with the Indians, Mariners and Giants",
                        "The five players are the person from Frenchboro, Ivan, the player with the Indians, the person with the Mariners and the graduate that plays for the Giants",
                        "The person who graduated in 2005 is from Unity",
                        "Lonnie plays for the Mariners"
                     ], [
                        noun([graduate], [graduates]),
                        noun([player], [players]),
                        noun([year], [years]),
                        noun([person], [persons]),
                        pn([michael]),
                        pn([eldon]),
                        pn([jackie]),
                        pn([ivan]),
                        pn([orlando]),
                        pn([dodgers]),
                        pn([york]),
                        pn([giants]),
                        pn([lonnie]),
                        pn([frenchboro]),
                        pn([indians]),
                        pn([mariners]),
                        pn([unity]),
                        tvPrep([graduated], [in], [graduate], [graduated]),
                        tvPrep([plays], [for], [play], [played]),
                        prep([from]),
                        prep([with])
                     ])).

problem(p5, problem(4, 5, [
                        "Mattie is 113 years old",
                        "The person who lives in Tehama is a native of either Kansas or Oregon",
                        "The Washington native is 1 year older than Ernesto",
                        "Roxanne is 2 years younger than the Kansas native",
                        "The person who lives in Zearing isn't a native of Alaska",
                        "The person who is 111 years old doesn't live in Plymouth",
                        "The Oregon native is either Zachary or the person who lives in Tehama",
                        "The person who lives in Shaver Lake is 1 year younger than Roxanne",
                        "The centenarian who lives in Plymouth isn't a native of Alaska",
                        "Of the person who lives in Tehama and Mattie, one is a native of Alaska and the other is from Kansas"
                     ], [
                        noun([year], [years]),
                        noun([person], [persons]),
                        noun([native], [natives]),
                        noun([centenarian], [centenarians]),
                        pn([mattie]),
                        pn([tehama]),
                        pn([kansas]),
                        pn([oregon]),
                        pn([washington]),
                        pn([ernesto]),
                        pn([roxanne]),
                        pn([zearing]),
                        pn([alaska]),
                        pn([plymouth]),
                        pn([zachary]),
                        pn([shaver, lake]),
                        tvPrep([lives], [in], [live], [lived]),
                        tvGap([is], [old], [are]),
                        prep([a, native, of]),
                        prep([from]),
                        comp(higher, [older, than]),
                        comp(lower, [younger, than])
                     ])).

% ADAPTED
% clue 4: of the two dogs who graduated in March and April -> of the dog who graduated in March and the dog who graduated in April, ...
problem(p6, problem(4, 5, [
                        "Officer Quinn's dog graduated in either March or April",
                        "Aries didn't graduate in March, May or June",
                        "The dog who graduated in March didn't go to Cole County",
                        "Of the dog who graduated in March and the dog who graduated in April, one went to Tanager County and the other was assigned to Officer Ingram",
                        "Aries wasn't assigned to Officer Ingram",
                        "Barca didn't go to Kermit County",
                        "The dog sent to Kermit County, the K-9 unit assigned to Officer Lyons and the canine who graduated in June are three different dogs",
                        "The dog assigned to Officer Salinas graduated 2 months after Aries",
                        "Of the dog sent to Sycamore County and Tinkerbell, one graduated in July and the other was assigned to Officer Underwood",
                        "Jaws graduated sometime after the dog sent to Tanager County",
                        "McGruff went to Sycamore County"
        ], [
                        pn([officer, quinn]),
                        pn([aries]),
                        pn([barca]),
                        pn([cole, county]),
                        pn([tanager, county]),
                        pn([officer, ingram]),
                        pn([kermit, county]),
                        pn([officer, salinas]),
                        pn([sycamore, county]),
                        pn([tinkerbell]),
                        pn([officer, underwood]),
                        pn([officer, lyons]),
                        pn([jaws]),
                        pn([mcgruff]),
                        noun([dog], [dogs]),
                        noun([k9, unit], [k9, units]),
                        noun([canine], [canines]),
                        noun([month], [months]),
                        tvPrep([graduated], [in], [graduate], [graduated]),
                        tvPrep([went], [to], [go], [went]),
                        tvPrep([assigned], [to], [assign], [assigned]),
                        tvPrep([sent], [to], [send], [sent]),
                        pnn([march], 3),
                        pnn([april], 4),
                        pnn([may], 5),
                        pnn([june], 6),
                        pnn([july], 7)
        ])).

% ADAPTED
% CLUE 1 and 2: May 18 -> in May
% CLUE 2: from Norway, from Canada -> The Norwegian, The Canadian
% CLUE 3, 8 and 9: ADD "birthday" to end of clue
problem(p7, problem(4, 5, [
                        "The Norwegian's birthday is in May",
                        %% "Of Bill and the traveler born in June, one is from Norway and the other is from Canada",
                        "Of Bill and the traveler born in June, one is the Norwegian and the other is the Canadian",
                        "Izzy's birthday is 1 month after the politician's birthday",
                        %% "The engineer is from France",
                        "The engineer is the frenchman",
                        "The South African's birthday is in either April or May",
                        "The musician's birthday isn't in April",
                        %% "Jeffrey is either French or South African",
                        "Jeffrey is either the Frenchman or the South African",
                        "The Canadian's birthday is 1 month after the surgeon's birthday",
                        "Harry's birthday is sometime before Bill's birthday"
        ], [
                        noun([birthday], [birthdays]),
                        noun([traveler], [travelers]),
                        noun([month], [months]),
                        pn([norwegian]),
                        pn([bill]),
                        pn([canadian]),
                        pn([izzy]),
                        pn([politician]),
                        pn([engineer]),
                        pn([frenchman]),
                        pn([musician]),
                        pn([jeffrey]),
                        pn([harry]),
                        pn([south, african]),
                        pn([surgeon]),
                        pnn([may], 5),
                        pnn([june], 6),
                        pnn([april], 4),
                        tvPrep([born], [in], [born], [born])
                        %% prep([in])
        ])).

% ADAPTED
% CLUE 4: orange -> the orange item
% CLUE 5 and 7: 5 fewer minutes -> 5 minutes fewer, moved to print to the end
% CLUE 9: just 10 -> 10 minutes to print
problem(p8, problem(4, 5, [
                        "The flower printed in 25 minutes",
                        "Adrienne's design took 15 minutes to print",
                        "The piece that printed in 30 minutes wasn't the mask",
                        "Of the item that printed in 10 minutes and Bertha's piece, one was the flower and the other was the orange item",
                        "The orange item required 5 minutes fewer than the yellow design to print",
                        "Adrienne's design was either the phone case or the item that printed in 30 minutes",
                        "The white item took 10 minutes more than Stella's piece",
                        "Raquel's piece wasn't the mask",
                        "Of the whistle and the blue piece, one took 30 minutes to print and the other took 10 minutes to print"
        ], [
                        noun([minute], [minutes]),
                        noun([design], [designs]),
                        noun([piece], [pieces]),
                        noun([item], [items]),
                        pn([flower]),
                        pn([mask]),
                        pn([phone, case]),
                        pn([whistle]),
                        pn([adrienne]),
                        pn([bertha]),
                        pn([raquel]),
                        pn([stella]),
                        pn([orange]),
                        pn([yellow]),
                        pn([blue]),
                        pn([white]),
                        tvPrep([printed], [in], [print], [printed]),
                        tvGap([took], [to, print], [take]),
                        tvGap([required], [to, print], [require]),
                        comp(lower, [fewer, than]),
                        comp(higher, [more, than])
        ])).

% ADAPTED!!!
% replaced "$... per share" with "... dollar per share"
problem(p11, problem(4, 5, [
                         "The utilities stock was 1 dollar per share less expensive than the stock Geraldo bought",
                         "GXTV was 2 dollar per share less expensive than QMZ",
                         "The financial stock wasn't purchased by Edith",
                         "PSTO sold for 29 dollar per share",
                         "The stock Abigail bought was either KMPP or JMO",
                         "The health-care stock was 2 dollar per share more expensive than the financial stock",
                         "The energy stock was less expensive than JMO",
                         "Heathcliff bought the real estate stock",
                         "Of QMZ and GXTV, one sold for 26 dollar per share and the other was in the health-care sector",
                         "Abigail didn't purchase the stock that sold for 25 dollar per share"
                     ], [
                         pn(sector, [utilities, stock]),
                         noun(price, [dollar, per, share], [dollars, per, share]),
                         % less expensive
                         noun(stock, [stock], [stocks]),
                         pn(person, [geraldo]),
                         tv(pred(person, stock), [bought], [buy]),
                         pn(stock, [gxtv]),
                         pn(stock, [qmz]),
                         pn(sector, [financial, stock]),
                         % does this work? (passive form)
                         ivpp(pred(stock, person), [purchased], [by], [purchase]),
                         pn(stock, [psto]),
                         ivpp(pred(stock, price), [sold], [for], [sell]),
                         pn(person, [abigail]),
                         pn(stock, [kmpp]),
                         pn(stock, [jmo]),
                         pn(sector, ['health-care', stock]),
                         % more expensive
                         pn(sector, [energy, stock]),
                         pn(person, [heathcliff]),
                         pn(sector, [real, estate, stock])
                         % in the health-care sector vs the health-care stock
                     ])).
