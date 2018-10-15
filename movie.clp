;----------------------------------------------------------------------------
;CLASSES
;----------------------------------------------------------------------------
(defclass PERSON
	(is-a USER)
	(role concrete)
	(slot companion))

(defclass MOVIE
	(is-a USER)
	(role concrete)
	(slot genre)
	(slot language)
	(slot suggested_movie)
	(slot type))

;----------------------------------------------------------------------------
; DEFAULT INSTANCES
;----------------------------------------------------------------------------

(definstances PERSON-INSTANCES
	(user of PERSON))

(definstances MOVIE-INSTANCES
	(movie_name of MOVIE))

;----------------------------------------------------------------------------
;INITIAL USER INPUTS AND VALIDATIONS
;----------------------------------------------------------------------------

(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
; RULE TO GET THE USER INPUT
(defrule GetCompanion(declare (salience 10))
    =>
    (printout t crlf)
    (printout t "-----------------------------------------------------------------------------" crlf)
    (printout t "------------------------ WELCOME TO THE MOVIE EXPERT ------------------------" crlf)
    (printout t "-----------------------------------------------------------------------------" crlf)
    (printout t crlf)    
    (send [user] put-companion
    (user-input-validation "Who are you going to watch with? (family/friends):  "
   		family friends)))
   
;----------------------------------------------------------------------------
;RULES OF THE EXPERT SYSTEM TO SELECT THE MOVIE
;----------------------------------------------------------------------------

; RULE TO WATCH MOVIE WITH FAMILY
(defrule select_language
	(and ?ins <- (object (is-a MOVIE))
	(object (is-a PERSON)))
	=> 
	(printout t crlf)
	(printout t "Which industry do you prefer" crlf crlf)
   	(send [movie_name] put-language
    (user-input-validation "Enter your preferred cinema industry (hollywood/bollywood):  "
    		hollywood bollywood)))

; RULE TO WATCH BOLLYWOOD MOVIE WITH FAMILY
(defrule select_genre
	(and ?ins <- (object (is-a MOVIE))
	(object (is-a PERSON)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable to watch with your family..." crlf crlf)
   	(send [movie_name] put-genre
    (user-input-validation "Enter your preferred genre (animated/action/comedy/horror/drama):  "
    		animated action comedy horror drama)))

; RULE TO WATCH BOLLYWOOD ANIMATED MOVIE WITH FAMILY
(defrule watch_bollywood_animated_with_family
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood animated movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Hanuman, Arjun, Mahabharat, Chota Bheem"))

; RULE TO WATCH BOLLYWOOD ACTION MOVIE WITH FAMILY
(defrule watch_bollywood_action_with_family
	(and ?ins <- (object (is-a MOVIE) (genre action) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood action movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Ek Tha Tiger, Parmanu, Special 26, Naam Shabana"))

; RULE TO WATCH BOLLYWOOD COMEDY MOVIE WITH FAMILY
(defrule watch_bollywood_comedy_with_family
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood comedy movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "OMG, Golmaal, Rocket Singh, Dostana"))

; RULE TO WATCH BOLLYWOOD HORROR MOVIE WITH FAMILY
(defrule watch_bollywood_horror_with_family
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood horror movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Bhool Bhulaiyaa, Stree, Bhootnath, Go Goa Gone"))

; RULES TO WATCH DRAMA MOVIES WITH FAMILY
(defrule watch_bollywood_drama_with_family
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood drama movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Kal Ho Na Ho, Kabhi Khushi Kabhi Gham, Dil to Pagal Hai, Dil Chahta Hai"))

; RULE TO WATCH BOLLYWOOD ANIMATED MOVIES friends
(defrule watch_bollywood_animated_friends
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood animated movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Kochadaiyyan, Makkhi, Roadside Romeo, Tarzan"))

; RULE TO WATCH BOLLYWOOD ACTION MOVIES friends
(defrule watch_bollywood_action_friends
	(and ?ins <- (object (is-a MOVIE) (genre action) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood action movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Singham, Race, Sultan, Kick"))


; RULE TO WATCH BOLLYWOOD COMEDY MOVIES friends
(defrule watch_bollywood_comedy_friends
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood comedy movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Housefull"))

; RULE TO WATCH BOLLYWOOD HORROR MOVIES friends
(defrule watch_bollywood_horror_friends
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood horror movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "13B, Pari, Bhoot, 1921"))


; RULE TO WATCH BOLLYWOOD DRAMA MOVIES friends
(defrule watch_bollywood_drama_friends
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood drama movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Dhadak, Andhadhun, Gold, Raazi"))


; /////////////////////////////////////////////////////
; Hollywood movie recommendations.


; RULE TO WATCH hollywood ANIMATED MOVIE WITH FAMILY
(defrule watch_hollywood_animated_with_family
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood animated movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Frozen, Finding Nemo, Up, Coco"))

; RULE TO WATCH hollywood ACTION MOVIE WITH FAMILY
(defrule watch_hollywood_action_with_family
	(and ?ins <- (object (is-a MOVIE) (genre action) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood action movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "American Sniper, Italian Job, Mr and Mrs Smith, White House Down"))

; RULE TO WATCH hollywood COMEDY MOVIE WITH FAMILY
(defrule watch_hollywood_comedy_with_family
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood comedy movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Wanderlust, Home Alone, We're the Millers, Ted"))

; RULE TO WATCH hollywood HORROR MOVIE WITH FAMILY
(defrule watch_hollywood_horror_with_family
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood horror movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Conjuring, Insidious, Paranormal Activity, Scream"))

; RULES TO WATCH hollywood DRAMA MOVIES WITH FAMILY
(defrule watch_hollywood_drama_with_family
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood drama movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Pursuit of Happyness, Arrival, Spotlight, Whiplash"))

; RULE TO WATCH hollywood ANIMATED MOVIES friends
(defrule watch_hollywood_animated_friends
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood animated movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Zootopia, Incredibles, Moana, Minions"))

; RULE TO WATCH hollywood ACTION MOVIES friends
(defrule watch_hollywood_action_friends
	(and ?ins <- (object (is-a MOVIE) (genre action) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood action movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Fast and Furious, Avengers, Mad Max, Dark Knight"))


; RULE TO WATCH hollywood COMEDY MOVIES friends
(defrule watch_hollywood_comedy_friends
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood comedy movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "21 Jump Street, Crazy Stupid Love, Deadpool, Hangover"))

; RULE TO WATCH hollywood HORROR MOVIES friends
(defrule watch_hollywood_horror_friends
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood horror movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Oculus, Shining, Ring, Exorcism"))


; RULE TO WATCH hollywood DRAMA MOVIES friends
(defrule watch_hollywood_drama_friends
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood drama movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Greatest Showman, Room, Prisoners, Gravity"))



;----------------------------------------------------------------------------
;PRINTS THE FINAL SUGGESSION	
;----------------------------------------------------------------------------

; RULE TO PRINT THE SUGGESTED MOVIE
(defrule choose_movie (declare (salience -1))
	(object (is-a MOVIE) (suggested_movie ?mov))
	=>
	(printout t crlf)
	(printout t "-----------------------------------------------------------------------------" crlf)
    (printout t "The recommended movie for you is: " ?mov crlf)
    (printout t "-----------------------------------------------------------------------------" crlf))
