:- module(problemPosterEvaluation, [problem/2]).

% ADAPTED!!!
% CLUE 1: added a [that]
% CLUE 3: active form
% CLUE 5: added a [that]
% CLUE 9: in the health-care sector -> the health-care stock
% PROBLEM: Sector and name both refer to the same stock => flat types doesn't work
problem(p11, problem(4, 5, [
                         "The utilities stock was $1 per share less expensive than the stock that Geraldo bought",
                         "GXTV was $2 per share less expensive than QMZ",
                         %% "The financial stock wasn't purchased by Edith",
                         "Edith didn't purchase the financial stock",
                         "PSTO sold for $29 per share",
                         %% "The stock Abigail bought was either KMPP or JMO",
                         "The stock that Abigail bought was either KMPP or JMO",
                         "The health-care stock was $2 per share more expensive than the financial stock",
                         "The energy stock was less expensive than JMO",
                         "Heathcliff bought the real estate stock",
                         %% "Of QMZ and GXTV, one sold for $26 per share and the other was in the health-care sector",
                         "Of QMZ and GXTV, one sold for $26 per share and the other was the health-care stock",
                         "Abigail didn't purchase the stock that sold for $25 per share"
                     ], [
                         noun([per, share], [per, shares]),
                         noun([stock], [stocks]),
                         pn([utilities, stock]),
                         pn([geraldo]),
                         pn([gxtv]),
                         pn([qmz]),
                         pn([financial, stock]),
                         pn([psto]),
                         pn([abigail]),
                         pn([kmpp]),
                         pn([jmo]),
                         pn([healthcare, stock]),
                         pn([energy, stock]),
                         pn([heathcliff]),
                         pn([real, estate, stock]),
                         pn([edith]),
                         tv([bought], [buy]),
                         tv([purchased], [purchase]),
                         % does this work? (passive form)
                         tvPrep([sold], [for], [sell], [sold]),
                         copGap([], [expensive]),
                         % in the health-care sector vs the health-care stock
                         comp(lower, [less, expensive, than]),
                         comp(lower, [more, expensive, than])
                     ])).


% ADAPTED
% integer prices
% clue 1: the order -> the person and paid, order doesn't exist
% clue 3: whoever -> the person who
% clue 3: had -> ordered (had is for drinking)
% clue 7: the one -> the person
problem(p12, problem(4, 5, [
                         %% "The order with the lemonade cost $1 more than the order with the water",
                         "The person with the lemonade paid $1 more than the person with the water",
                         "Homer paid $7",
                         "Glen paid $3 less than the person who ordered the sloppy joe",
                         "Wallace didn't have the iced tea",
                         "Of the diner who paid $6 and Homer, one ordered the spaghetti and the other drank the water",
                         "Oliver ordered the hamburger",
                         "The five diners were the person who ordered the turkey plate, Oliver, Glen, the person who got the iced tea and the person who paid $5",
                         "Glen didn't have the orange soda"
                     ], [
                         %% noun([order], [orders]),
                         noun([person], [people]),
                         noun([diner], [diners]),
                         pn([lemonade]),
                         pn([water]),
                         pn([homer]),
                         pn([glen]),
                         pn([wallace]),
                         pn([oliver]),
                         pn([sloppy, joe]),
                         pn([iced, tea]),
                         pn([spaghetti]),
                         pn([hamburger]),
                         pn([turkey, plate]),
                         pn([orange, soda]),
                         tv([cost], [cost]),
                         tv([paid], [pay]),
                         tv([had], [have]),
                         tv([ordered], [order]),
                         tv([drank], [drink]),
                         tv([got], [get]),
                         prep([with])
                     ])).

% ADAPTED
% clue 6: lucas's puppet -> the puppet from Lucas
% clue 7: nicole's puppet -> the puppet from Nicole
% clue 8: isabel's -> from isabel
% clue 9: the one -> the puppet
% clue 10: of the $1000 and $1250 dummies -> of the $1000 dummy and the $1250 dummy
% clue 10: herman's -> from herman
problem(p13, problem(4, 5, [
                         "Kelly's piece didn't cost $1250",
                         "Valencia cost somewhat more than Isabel's dummy",
                         "The puppet going to Vancouver, the $750 dummy and the $1500 piece are three different dummies",
                         "Waldarama didn't cost $750 or $1500",
                         "Kelly's puppet isn't going to Ypsilanti",
                         "The dummy going to Mexico City is either Tombawomba or the puppet from Lucas",
                         "The puppet from Nicole, the $1000 piece and the puppet going to Ypsilanti are three different dummies",
                         "Of the $750 puppet and the piece going to Mexico City, one is Tombawomba and the other is from Isabel",
                         "The puppet going to Ypsilanti cost $250 more than the puppet going to St. Moritz.",
                         "Of the $1000 dummy and the $1250 dummy, one is from Herman and the other is going to Mexico City",
                         "Sniffletoe sold for $1000"
                     ], [
                         noun([piece], [pieces]),
                         noun([dummy], [dummies]),
                         noun([puppet], [puppets]),
                         pn([kelly]),
                         pn([valencia]),
                         pn([isabel]),
                         pn([vancouver]),
                         pn([waldarama]),
                         pn([ypsilanti]),
                         pn([tombawomba]),
                         pn([lucas]),
                         pn([mexico, city]),
                         pn([nicole]),
                         pn([st, moritz]),
                         pn([herman]),
                         pn([sniffletoe]),
                         tv([cost], [cost]),
                         %% copGap([going, to], []),
                         tvPrep([go], [to], [go], [going]),
                         tvPrep([sold], [for], [sell], [sold]),
                         prep([from])
                     ])).
