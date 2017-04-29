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
                         prep([with]),
                         comp(higher, [more, than]),
                         comp(lower, [less, than])
                     ])).
