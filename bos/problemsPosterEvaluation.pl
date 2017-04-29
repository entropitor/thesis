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

% 1 + 3) the one -> the shell
% 11) monk's fin -> monk's fin shell
% use "is in" because "is" doesn't work
% use "discovered in" instead of "found in" for years (only 1 type per verb)
problem(p14, problem(4, 5, [
                         "The baby's ear shell was discovered 3 years before the shell from Jamaica",
                         "The seashell found in Puerto Rico isn't in black and white",
                         "The shell discovered in 1992 isn't in blue and white",
                         "The blue and white shell, the shark eye shell and the shell found in Barbuda are three different specimens",
                         "The piece found in Saint Lucia was discovered 6 years after the honey cerith shell",
                         "Of the baby's ear shell and the black and white shell, one was discovered in 2004 and the other was found in Saint Lucia",
                         "The monk's fin shell isn't in black and white",
                         "The seashell discovered in 2001 is in brown and yellow",
                         "The shell discovered in 1995 is in pink and yellow",
                         "The baby's ear shell was discovered 3 years after the shell found in Aruba",
                         "The shell discovered in 2001 is either the monk's fin shell or the coquina"
                     ], [
                         noun([shell], [shells]),
                         noun([seashell], [seashells]),
                         noun([specimen], [specimens]),
                         noun([year], [years]),
                         noun([piece], [pieces]),
                         pn([baby, s, ear, shell]),
                         pn([jamaica]),
                         pn([seashell]),
                         pn([puerto, rico]),
                         pn([black, and, white]),
                         pn([blue, and, white]),
                         pn([shark, eye, shell]),
                         pn([barbuda]),
                         pn([saint, lucia]),
                         pn([honey, cerith, shell]),
                         pn([monk, s, fin, shell]),
                         pn([brown, and, yellow]),
                         pn([pink, and, yellow]),
                         pn([coquina]),
                         pn([aruba]),
                         tvPrep([found], [in], [find], [found]),
                         tvPrep([discovered], [in], [discover], [discovered]),
                         prep([in]),
                         prep([from])
                     ])).

% whoever -> the person who
% clue 6: drop the also
% clue 10: of the two presentation -> of the presentation on ... and the presentation on ...
% clue 11,12: the one -> the presentation
% clue 13: the lowest grade -> D
%% % clue 11: got -> received (only 1 meaning per verb)

% PROBLEM: intermediate type "presentation" can not be represented
problem(p15, problem(4, 5, [
                         "The student who got the B-minus talked about Augustus",
                         "Johnnie, the student who gave the presentation on Augustus and the person who got the B-plus were three different students",
                         "Gina didn't get the B-plus",
                         "The student who spoke for 10 minutes didn't get the C-Minus",
                         "Gina didn't talk about Galerius",
                         "Yolanda didn't give a presentation on Galerius",
                         "Yolanda, the student who gave the presentation on Caligula and the student who got the B-minus were three different children",
                         "Gina didn't get the A",
                         "The student who spoke for 8 minutes talked about Galerius",
                         %% "Of the presentation on Augustus and the presentation on Caligula, one was given by Catherine and the other lasted for 10 minutes",
                         %% "The presentation that received the A was 4 minutes shorter than the presentation on Caligula",
                         %% "The B-minus presentation was 6 minutes longer than the presentation that got the D",
                         %% "The talk on Nero was 2 minutes shorter than the presentation that got the D"
                         "Of the person that talked about Augustus and the student that talked about Caligula, one was Catherine and the other spoke for 10 minutes",
                         "The child that got the A spoke for 4 minutes shorter than the student on Caligula",
                         "The B-minus student spoke 6 minutes more than the student that got the D",
                         "The student on Nero spoke 2 minutes less than the child that got the D"
                     ], [
                         noun([student], [students]),
                         noun([person], [persons]),
                         noun([minute], [minutes]),
                         noun([child], [children]),
                         pn([augustus]),
                         pn([caligula]),
                         pn([galerius]),
                         pn([nero]),
                         pn([johnnie]),
                         pn([gina]),
                         pn([yolanda]),
                         pn([catherine]),
                         pn([a]),
                         pn([bplus]),
                         pn([bminus]),
                         pn([cminus]),
                         pn([d]),
                         tv([got], [get]),
                         tvPrep([talked], [about], [talk], [talked]),
                         tvPrep([spoke], [for], [speak], [spoken]),
                         tvPrep([given], [by], [give], [given]),
                         tvPrep([lasted], [for], [last], [lasted]),
                         copGap([], [long]),
                         comp(lower, [shorter, than]),
                         comp(higher, [longer, than]),
                         %% noun([presentation], [presentations]),
                         %% noun([talk], [talks]),
                         %% tv([gave], [give]),
                         %% tv([received], [receive]),
                         %% prep([on])
                         tvPrep([gave, the, presentation], [on], [give, the, presentation], [given, the, presentation]),
                         tvPrep([gave, a, presentation], [on], [give, a, presentation], [given, a, presentation]),
                         prep([on])
                     ])).

% 1, 7) the one -> juggler
% 10) dropped "always"
problem(p16, problem(4, 5, [
                         "The juggler who went fourth was either the performer from Quasqueton or the juggler who used rings",
                         "The juggler who used flashlights performed one spot after the person who used mobile phones",
                         "The performer from Kingsburg performed one spot before Howard",
                         "Otis wasn't from Carbon",
                         "Of the performer who went second and the juggler who used rings, one was from Carbon and the other is Howard",
                         "The performer who went third, Gerald and the person from Kingsburg are three different people",
                         "Floyd was either the juggler who went second or the juggler from Quasqueton",
                         "The person who went third used rings",
                         "The juggler who went second wasn't from Nice",
                         "Floyd juggles rubber balls"
                     ], [
                         noun([juggler], [jugglers]),
                         noun([performer], [performers]),
                         noun([person], [people]),
                         noun([spot], [spots]),
                         pn([quasqueton]),
                         pn([rings]),
                         pn([flashlights]),
                         pn([mobile, phones]),
                         pn([kingsburg]),
                         pn([howard]),
                         pn([otis]),
                         pn([carbon]),
                         pn([gerald]),
                         pn([nice]),
                         pn([floyd]),
                         pn([rubber, balls]),
                         pnn([second], 2),
                         pnn([third], 3),
                         pnn([fourth], 4),
                         tv([went], [go]),
                         tv([used], [use]),
                         tv([performed], [perform]),
                         tv([juggles], [juggle]),
                         prep([from])
                     ])).

% 2, 9) the one -> the outing
% 5, 7) Oscar's -> the tour from Oscar

% begin is for time, start for place
% remove group: group of 6 -> tour with 6
% add 10 to group sizes: they overlap!!!
% Add extra type!!!
problem(p17, problem(5, 5, [
                         "The 11 am tour won't start at high park",
                         "Zachary's outing will begin 1 hour before the outing starting at Casa Loma",
                         "The tour with 17 people will begin 3 hours before the tour with 16",
                         "The tour with 12 people will start at Yorkville",
                         "Zachary's trip will begin 3 hours before the trip from Janice",
                         "Zachary's outing will begin 2 hours before the tour starting at Yorkville",
                         "Of the 7 am tour and the tour from Oscar, one will start at Yorkville and the other will start at City Hall",
                         "The outing from Whitney isn't with 15 people",
                         "The Yorkville tour, the tour beginning at 9 am and the outing with 15 people are three different tours"
                     ], [
                         noun([am], [ams]),
                         noun([tour], [tours]),
                         noun([outing], [outings]),
                         noun([person], [people]),
                         noun([hour], [hours]),
                         %% noun([group], [groups]),
                         noun([trip], [trips]),
                         pn([high, park]),
                         pn([casa, loma]),
                         pn([zachary]),
                         pn([yorkville]),
                         pn([janice]),
                         pn([oscar]),
                         pn([city, hall]),
                         pn([whitney]),
                         tvPrep([start], [at], [start], [starting]),
                         tvPrep([begin], [at], [begin], [beginning]),
                         prep([with]),
                         prep([from]),
                         prep([of])
                     ])).

% "the one" -> the person
% dropped "exactly"
% "... fewer facebook friends than" -> "... facebook friends less than"
% use different verbs!!!
problem(p93, problem(4, 5, [
                         "Opal is connected to 64 LinkedIn connections",
                         "The person followed by 809 Twitter followers, the person with 140 facebook friends and the person connected to 78 linkedin connections are three different people",
                         "Of rosie and neil, one is connected to 68 linkedin connections and the other is followed by 789 twitter followers",
                         "The person connected to 57 linkedin connections has 10 facebook friends less than the person followed by 715 twitter followers",
                         "Arnold isn't followed by 589 twitter followers",
                         "The person followed by 809 twitter followers isn't connected to 68 linkedin connections",
                         "Of the person connected to 57 linkedin connections and arnold, one has 140 facebook friends and the other is followed by 789 twitter followers",
                         "opal doesn't have 150 facebook friends",
                         "the person connected to 57 linkedin connections has 10 facebook friends less than georgia",
                         "The person with 130 facebook friends is either arnold or the person followed by 715 twitter followers",
                         "the person followed by 789 twitter followers has somewhat less than rosie"
                     ], [
                         noun([linkedin, connection], [linkedin, connections]),
                         noun([twitter, follower], [twitter, followers]),
                         noun([facebook, friend], [facebook, friends]),
                         noun([person], [people]),
                         pn([opal]),
                         pn([rosie]),
                         pn([neil]),
                         pn([arnold]),
                         pn([georgia]),
                         tvPrep([connected], [to], [connect], [connected]),
                         tvPrep([followed], [by], [follow], [followed]),
                         tv([has], [have]),
                         prep([with])
                     ])).

%% problem(foo, problem(4, 5, [], [])).