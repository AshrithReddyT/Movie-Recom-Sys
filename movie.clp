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
    (user-input-validation "Enter your preferred language (hollywood/bollywood):  "
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
	(send ?ins put-suggested_movie "Hanuman"))

; RULE TO WATCH BOLLYWOOD ACTION MOVIE WITH FAMILY
(defrule watch_bollywood_action_with_family
	(and ?ins <- (object (is-a MOVIE) (genre action) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood action movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Ek Tha Tiger"))

; RULE TO WATCH BOLLYWOOD COMEDY MOVIE WITH FAMILY
(defrule watch_bollywood_comedy_with_family
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood comedy movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "OMG"))

; RULE TO WATCH BOLLYWOOD HORROR MOVIE WITH FAMILY
(defrule watch_bollywood_horror_with_family
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood horror movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Bhool Bhulaiyaa"))

; RULES TO WATCH DRAMA MOVIES WITH FAMILY
(defrule watch_bollywood_drama_with_family
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language bollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood drama movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Kal Hoe Na Hoe"))

; RULE TO WATCH BOLLYWOOD ANIMATED MOVIES friends
(defrule watch_bollywood_animated_friends
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood animated movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "anim_friends"))

; RULE TO WATCH BOLLYWOOD ACTION MOVIES friends
(defrule watch_bollywood_action_friends
	(and ?ins <- (object (is-a MOVIE) (genre action) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood action movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Singham"))


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
	(send ?ins put-suggested_movie "horror_friends"))


; RULE TO WATCH BOLLYWOOD DRAMA MOVIES friends
(defrule watch_bollywood_drama_friends
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language bollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable bollywood drama movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Zindgi nah milegi dobara"))


; /////////////////////////////////////////////////////
; Hollywood movie recommendations.


; RULE TO WATCH hollywood ANIMATED MOVIE WITH FAMILY
(defrule watch_hollywood_animated_with_family
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood animated movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Frozen"))

; RULE TO WATCH hollywood ACTION MOVIE WITH FAMILY
(defrule watch_hollywood_action_with_family
	(and ?ins <- (object (is-a MOVIE) (genre action) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood action movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "American Sniper"))

; RULE TO WATCH hollywood COMEDY MOVIE WITH FAMILY
(defrule watch_hollywood_comedy_with_family
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood comedy movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Wanderlust"))

; RULE TO WATCH hollywood HORROR MOVIE WITH FAMILY
(defrule watch_hollywood_horror_with_family
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood horror movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Conjuring"))

; RULES TO WATCH hollywood DRAMA MOVIES WITH FAMILY
(defrule watch_hollywood_drama_with_family
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language hollywood))
	(object (is-a PERSON) (companion family)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood drama movie to watch with your family..." crlf crlf)
	(send ?ins put-suggested_movie "Pursuit of happiness"))

; RULE TO WATCH hollywood ANIMATED MOVIES friends
(defrule watch_hollywood_animated_friends
	(and ?ins <- (object (is-a MOVIE) (genre animated) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood animated movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "UP"))

; RULE TO WATCH hollywood ACTION MOVIES friends
(defrule watch_hollywood_action_friends
	(and ?ins <- (object (is-a MOVIE) (genre action) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood action movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Fast and furious"))


; RULE TO WATCH hollywood COMEDY MOVIES friends
(defrule watch_hollywood_comedy_friends
	(and ?ins <- (object (is-a MOVIE) (genre comedy) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood comedy movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "21 Jump street"))

; RULE TO WATCH hollywood HORROR MOVIES friends
(defrule watch_hollywood_horror_friends
	(and ?ins <- (object (is-a MOVIE) (genre horror) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood horror movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Oculus"))


; RULE TO WATCH hollywood DRAMA MOVIES friends
(defrule watch_hollywood_drama_friends
	(and ?ins <- (object (is-a MOVIE) (genre drama) (language hollywood))
	(object (is-a PERSON) (companion friends)))
	=> 
	(printout t crlf)
	(printout t "Let me select a movie suitable hollywood drama movie to watch friends..." crlf crlf)
	(send ?ins put-suggested_movie "Greatest Showman"))


;; ; RULE TO WATCH MOVIE friends
;; (defrule watch_friends
;; 	?ins <- (object (is-a PERSON) (companion friends))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select a movie that you can enjoy friends..." crlf crlf)
;;     (send [movie_name] put-type
;;   	(user-input-validation "Do you want to watch a TV show or a Movie? (tv/movie): " 
;;   		tv movie)))
   	 
;; ; RULE TO WATCH MOVIE WITH PARTNER
;; (defrule watch_with_partner
;; 	?ins <- (object (is-a PERSON) (companion partner))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select a movie suitable to watch with your partner..." crlf crlf)
;; 	(send [movie_name] put-year
;;     (user-input-validation "Would you like to watch a new or old movie? (new/old): "
;;          new old)))
   	 
;; ; RULE TO WATCH TV SHOWS friends
;; (defrule watch_tv
;; 	(and ?ins <- (object (is-a MOVIE) (type tv))
;; 	(object (is-a PERSON)(companion friends)))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select a tv show that you can watch friends..." crlf crlf)
;; 	(send [movie_name] put-genre
;;     (user-input-validation "Enter your preferred genre (comedy/action/thriller):  "
;;          comedy action thriller)))
   	
;; ; RULE TO WATCH NEW MOVIE WITH PARTNER
;; (defrule watch_new_movies
;; 	(and ?ins <- (object (is-a MOVIE) (year new))
;; 	(object (is-a PERSON)(companion partner)))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select a new romantic movie to watch with your partner..." crlf crlf)
;;    	(send [movie_name] put-genre
;;    	(user-input-validation "Enter your preferred genre (crime/comedy/drama):  "
;;          crime comedy drama)))

;; ; RULE TO WATCH ANIMATION MOVIE WITH FAMILY
;; (defrule watch_animated_movies_with_family
;; 	(and ?ins <- (object (is-a PERSON) (companion family))
;; 	(object (is-a MOVIE) (genre animation) (language hollywood)))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select an animated movie you can watch with your family..." crlf crlf)
;;    	(send [movie_name] put-studio
;;    	(user-input-validation "Select your favorite Studio (disney/pixar/others): "
;;          disney pixar others)))

;; ; RULE TO WATCH FANTASY MOVIE WITH FAMILY
;; (defrule watch_fantasy_movies_with_family
;; 	(and ?ins <- (object (is-a PERSON) (companion family))
;; 	(object (is-a MOVIE) (genre fantasy)))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select a fantasy movie you can watch with your family..." crlf crlf)
;;    	(send [user] put-fantasy
;;    	(user-input-validation "What is your preferred fantasy? (magic/monsters/ghosts):  "
;;          magic monsters ghosts)))
   	 
;; ; RULE TO WATCH HORROR MOVIE WITH FAMILY
;; (defrule watch_horror_movies_with_family
;; 	(and ?ins <- (object (is-a PERSON) (companion family))
;; 	(object (is-a MOVIE) (genre horror)))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select a horror movie you can watch with your family..." crlf crlf)   	(send [user] put-fear
;;    	(user-input-validation "What is your deepest fear? (killers/ghosts/zombies):  "
;;          killers ghosts zombies)))
 
;; ; RULE TO WATCH ADVENTURE MOVIE WITH FAMILY
;; (defrule watch_adventure_movies_with_family
;; 	(and ?ins <- (object (is-a PERSON) (companion family))
;; 	(object (is-a MOVIE) (genre adventure)))
;; 	=> 
;; 	(printout t crlf)
;; 	(printout t "Let me select an adventure movie you can watch with your family..." crlf crlf) 
;;    	(send [user] put-explore
;;    	(user-input-validation "Where would you like to explore? (jungle/space/ocean):  "
;;          jungle space ocean)))
 
;; ; RULE TO WATCH DISNEY MOVIE WITH FAMILY
;; (defrule watch_disney_movies
;; 	(and ?ins <- (object (is-a MOVIE) (studio disney))
;; 	(object (is-a PERSON) (companion family)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "Lion King"))

;; ; RULE TO WATCH PIXAR MOVIE WITH FAMILY
;; (defrule watch_pixar_movies
;; 	(and ?ins <- (object (is-a MOVIE) (studio pixar))
;; 	(object (is-a PERSON) (companion family)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "Toy Story"))

;; ; RULE TO WATCH OTHERS MOVIE WITH FAMILY
;; (defrule watch_others
;; 	(and ?ins <- (object (is-a MOVIE) (studio others))
;; 	(object (is-a PERSON) (companion family)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "The Land Before Time"))

;; ; RULE TO WATCH MAGIC FANTASY MOVIE WITH FAMILY
;; (defrule watch_fantasy_magic
;; 	(and ?ins <- (object (is-a PERSON) (fantasy magic))
;; 	(object (is-a MOVIE) (genre fantasy)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Harry Potter"))
	
;; ; RULE TO WATCH MONSTERS FANTASY MOVIE WITH FAMILY
;; (defrule watch_fantasy_monsters
;; 	(and ?ins <- (object (is-a PERSON) (fantasy monsters))
;; 	(object (is-a MOVIE) (genre fantasy)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Hotel Transylvania"))
	
;; ; RULE TO WATCH GHOSTS FANTASY MOVIE WITH FAMILY
;; (defrule watch_fantasy_ghosts 
;; 	(and ?ins <- (object (is-a PERSON) (fantasy ghosts))
;; 	(object (is-a MOVIE) (genre fantasy)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Casper"))

;; ; RULE TO WATCH KILLERS HORROR MOVIE WITH FAMILY
;; (defrule watch_fear_killers 
;; 	(and ?ins <- (object (is-a PERSON) (fear killers))
;; 	(object (is-a MOVIE) (genre horror)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "The Texas Chainsaw Massacre"))
	
;; ; RULE TO WATCH GHOSTS HORROR MOVIE WITH FAMILY
;; (defrule watch_fear_ghosts  
;; 	(and ?ins <- (object (is-a PERSON) (fear ghosts))
;; 	(object (is-a MOVIE) (genre horror)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "The Conjuring"))
	
;; ; RULE TO WATCH ZOMBIE HORROR MOVIE WITH FAMILY
;; (defrule watch_fear_zombies  
;; 	(and ?ins <- (object (is-a PERSON) (fear zombies))
;; 	(object (is-a MOVIE) (genre horror)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "28 Days Later"))

;; ; RULE TO WATCH JUNGLE ADVENTURE MOVIE WITH FAMILY
;; (defrule watch_explore_jungle 
;; 	(and ?ins <- (object (is-a PERSON) (explore jungle))
;; 	(object (is-a MOVIE) (genre adventure)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Tarzan"))
	
;; ; RULE TO WATCH SPACE ADVENTURE MOVIE WITH FAMILY
;; (defrule watch_explore_space
;; 	(and ?ins <- (object (is-a PERSON) (explore space))
;; 	(object (is-a MOVIE) (genre adventure)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "WALL-E"))
	
;; ; RULE TO WATCH OCEAN ADVENTURE MOVIE WITH FAMILY
;; (defrule watch_explore_ocean
;; 	(and ?ins <- (object (is-a PERSON) (explore ocean))
;; 	(object (is-a MOVIE) (genre adventure)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Finding Nemo"))
	
;; ; RULE TO WATCH OLD MOVIE WIH PARTNER
;; (defrule watch_old_movies
;; 	(and ?ins <- (object (is-a MOVIE) (year old))
;; 	(object(is-a PERSON)(companion partner)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "Notorious") )

;; ; RULE TO WATCH MOVIE NEW CRIME WITH PARTNER
;; (defrule watch_new_crime_movie_with_partner
;; 	(and ?ins <- (object (is-a PERSON) (companion partner))
;; 	(object (is-a MOVIE) (genre crime)(year new)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Mr. & Mrs. Smith"))
	
;; ; RULE TO WATCH MOVIE NEW COMEDY WITH PARTNER
;; (defrule watch_new_comedy_movie_with_partner
;; 	(and ?ins <- (object (is-a PERSON) (companion partner))
;; 	(object (is-a MOVIE) (genre comedy)(year new)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "The Girl Next Door"))
	
;; ; RULE TO WATCH MOVIE NEW DRAMA WITH PARTNER
;; (defrule watch_new_drama_movie_with_partner
;; 	(and ?ins <- (object (is-a PERSON) (companion partner))
;; 	(object (is-a MOVIE) (genre drama)(year new)))
;; 	=> 
;; 	(send [movie_name] put-suggested_movie "Casablanca"))
	
;; ; RULE TO WATCH TV COMEDY
;; (defrule watch_tv_comedy
;; 	(and ?ins <- (object (is-a MOVIE) (type tv)(genre comedy))
;; 	(object (is-a PERSON) (companion friends)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "F.R.I.E.N.D.S"))
	
;; ; RULE TO WATCH TV ACTION
;; (defrule watch_tv_action
;; 	(and ?ins <- (object (is-a MOVIE) (type tv)(genre action))
;; 	(object (is-a PERSON) (companion friends)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "Prison Break"))
	
;; ; RULE TO WATCH TV THRILLER
;; (defrule watch_tv_thriller
;; 	(and ?ins <- (object (is-a MOVIE) (type tv)(genre thriller))
;; 	(object (is-a PERSON) (companion friends)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "Dexter"))
	
;; ; RULE TO WATCH MOVIE friends
;; (defrule watch_movie_friends
;; 	(and ?ins <- (object (is-a MOVIE) (type movie))
;; 	(object (is-a PERSON) (companion friends)))
;; 	=> 
;; 	(send ?ins put-suggested_movie "The Prestige")
;; 	(printout t crlf)
;; 	(printout t "Let me select a movie you can enjoy friends..." crlf)) 
	
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