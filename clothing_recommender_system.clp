;Template for storing user preferences
(deftemplate user-pref
(slot occasion)
(slot range)
(slot category)
(slot gender))

;Question template
(deftemplate question 
(slot text)
(slot type)
(slot ident))

;Answer template
(deftemplate answer
(slot ident)
(slot answer))


;perform validation based on the type of input
(deffunction is-type (?answer ?type) 
    "Checking the answer type"
    (if (eq ?type male-female )then
        (return (or (eq ?answer male)(eq ?answer female)))
    else (if (eq ?type category)then
        	(return (or (eq ?answer tshirts)(eq ?answer jeans)(eq ?answer shirts)(eq ?answer shorts)(eq ?answer dresses)(eq ?answer pants)))
	else (if (eq ?type occasion)then
        	(return (or (eq ?answer formals)(eq ?answer casual)(eq ?answer party)) )      
    	  else (if(eq ?type range)then
                (return(or (or (eq ?answer A)(eq ?answer a))(or (eq ?answer B)(eq ?answer b))))
           	else(if(or(eq ?type age))then
                 (return(numberp ?answer))))
    	 )
 	)
 )

;Read answer and proceed if the type is correct. If not, request for correct input     
(deffunction ask-user(?question ?type)
    "Ask questions and return answer"
    (bind ?answer "") 
    (while (not (is-type ?answer ?type))do
        (printout t ?question " " )
        (if (eq ?type male-female )then
            (printout t "( male or female ) "))
        (if (eq ?type range)then
            (printout t  crlf)
            (printout t "What price range do you prefer?," crlf " A. ($5-$50) " crlf " B. ($50-$300)" crlf "Please enter one of the range from above (A or B) ")) 
        (if (eq ?type category )then
            (printout t  crlf)
            (printout t "<Choose one of the following clothing categories>" crlf "( tshirts, shirts,blouses, dresses, jeans,coats, shorts,pants) "))
        (if (eq ?type occasion)then
	    (printout t  crlf)
            (printout t "<Please choose one of the following ocassions>" crlf "( formals,casual,party ) "))
        (if (eq ?type age )then
            (printout t crlf "Please enter your age (in numbers) "))
        (bind ?answer (read))
     )        
    (return ?answer)
 )       


;function for welcome message
(defrule welcome
(declare (salience 100))   
    =>
    (printout t crlf crlf " Enter your name ")
    (bind ?name (read))
    (assert ( answer (ident name)(answer ?name)))
    (printout t crlf "##########################################" crlf crlf)
    (printout t "Hi " ?name "." crlf)
    (printout t "Welcome to H&M clothing recommender system " crlf)
    (printout t "Please answer the following questions and clothes we will recommend clothes based on your liking " crlf)
    (printout t "Please note that all the clothes recommended will be available in multiple colors" crlf)
    (printout t crlf crlf "############################################" crlf crlf))

;rule that binds asserts the answer into the memeory
(defrule ask-question-by-id
 "Ask a question and assert the answer" 
(question (ident  ?id)(text ?text)(type ?type))
(not (answer (ident ?id)))
 ?ask <- (ask ?id)
 =>                        
 (bind ?answer (ask-user ?text ?type))
 (assert (answer (ident ?id)(answer ?answer)))
 (retract ?ask)
 (return)            
  )

;ask price range 
(defrule request-budget	=>(assert(ask range))) 

;ask the gender
(defrule request-gender=>(assert (ask gender))) 


(defrule request-category=>(assert(ask cateogry)))

(defrule request-occasion=>(assert(ask occasion)))
(defrule request-age=>(assert(ask age)))    
 
  
  
(defrule assert-user-fact

  (answer (ident gender)(answer ?i))
  (answer (ident category)(answer ?a))
  (answer (ident range)(answer ?p))
  (answer (ident name)(answer ?name))       
    =>   
   (assert (user-pref (gender ?i)(category ?a)(range ?p)(name ?name)))
 )     



/* Recommending clothes to user based on his/her category and Budget
*/ 
 
(defrule recommend1

   (user-pref(gender male)(category tshirts)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  => 
   (printout t crlf " "?name "You can choose one of the following t-shirts:" crlf "i.Silk blend tshirt <$59.99>" "ii.Cotton polo shirt <$55.99> " "iii.Hooded sports tshirt <$60.99> " )
     )
(defrule recommend2

   (user-pref(gender male)(category tshirts)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following t-shirts:" crlf "i.Cotton tshirt <$9.99> ii.Regular fit v-neck tshirt <$5.99> iii.3 pack slim fot tshirts <$24.99> " )
    )  
(defrule recommend3
 
   (user-pref(gender male)(category jeans)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following jeans:" crlf "i.Slim jeans <$69.99> ii.Straight slim selvedge jeans <$69.99> iii.Skinny biker jeans <$55.99> " )
    )  
(defrule recommend4

   (user-pref(gender male)(category jeans)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following jeans" crlf "i.Skinny jeans <$24.99> ii.Slim straight jeans <$34.99> iii.Tapered jeans <$46.99> " crlf)
    ) 
(defrule recommend5

   (user-pref(gender male)(category shirts)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following shirts" crlf "i.Linen band collar shirt <$59.99> ii.Denim Shirt <$55.99> iii.Slim fit stretch shirt <$65.99> " crlf )
    )     
(defrule recommend6
  
   (user-pref(gender male)(category shirts)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following shirts" crlf "i.Striped shirt <$39.99> ii.Premium oxford cotton shirt <$48> iii.Regular fit cotton shirt <$29.99> " crlf)
    )   
(defrule recommend7
  
  (user-pref(gender female)(category dresses)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following dresses- <for Women>" crlf "i.Pleated dress <$99.99> ii.Calf length lace dress <$79.99> iii.Sleeveless sequined dress <$299.99> " crlf )
    )     
(defrule recommend8
 
  (user-pref(gender female)(category dresses)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name  "You can choose one of the following dresses- <for Women>" crlf "i.A-line dress <$44.99> ii.Fitted dress <$36.99> iii.short dress <$30.99> " crlf)
    )      
(defrule recommend9
 
  (user-pref(gender male)(category pants)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following pants" crlf "i.Skinny fit wool tuxedo pants <$80.99> ii.Suit pants <$65.99> iii.Slim fit tuxedo pants <$54.99> " crlf)
   )
(defrule recommend10
 
  (user-pref(gender male)(category pants)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following pants" crlf "i.Skinny fit suit pants <$49.97> ii.Muscle fit suit pants <$39.97> iii.Skinny fit wool suit pants <$49.99> " crlf)
   )
(defrule recommend11
   
  (user-pref(gender female)(category tshirts)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following tops- <for Women>" crlf "i.Silk camisole top <$59.99> ii.Lace bodysuit<$69.99> iii.Wool jersey top <$60.99> " crlf)
   )    
(defrule recommend12
  
  (user-pref(gender female)(category tshirts)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following tops- <for Women>" crlf "i.V-neck top <$19.99> ii.Short jersey top <$12.99> iii.One shoulder top  <$11.99> " crlf)
   )  
(defrule recommend13
   
  (user-pref(gender female)(category jeans)(name ?name)(range ?r&:(or (eq ?r b)(eq ?r B))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following jeans- <for Women>" crlf "i.Straight jeans <$68.99> ii.Shaping skinny high jeans<$79.99> iii.Slim ankle jeans <$60.99> " crlf)
   )    
(defrule recommend14
 
  (user-pref(gender female)(category jeans)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following jeans- <for Women>" crlf "i.Girlfriend regular jeans <$39.99> ii.Embrace high ankle jeans <$40.99> iii.Skinny high jeans  <$24.99> " crlf )
   )  
(defrule recommend15
  
  (user-pref(gender female)(category shirts)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following shirts- <for Women>" crlf "i.Silk shirt <$129.99> ii. Silk tuxedo shirt<$125.99> iii.Cotton shirt <$60.99> " crlf )
   )  
(defrule recommend16
   
  (user-pref(gender female)(category shirts)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " You can choose one of the following  shirts- <for Women>" crlf "i.V-neck shirt<$14.99> ii. Cotton shirt<$12.99> iii. Viscose shirt <$37.99> " crlf)
   )  
(defrule recommend17
   
  (user-pref(gender female)(category pants)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following pants- <for Women>" crlf "i. Wide leg pants <$68.99> ii.Cashmere joggers<$124.99> iii.Straight-leg pants<$99> " crlf )
   )  
(defrule recommend18
 
  (user-pref(gender female)(category pants)(name ?name)(range ?r&:(or (eq ?r a)(eq ?r A))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following pants- <for Women>" crlf "i.Ankle length slacks<$15.99> ii.Faux leather pants <$44> iii. Cotton blend joggers<$12.99> " crlf)
   )  
(defrule recommend19
 
  (user-pref(gender female)(category shorts)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following shorts- <for Women>" crlf "i.Silk blend shorts (multiple colors) <$69.99> ii.Flonced shorts <$59.99> iii.Denim overall shorts <$55.99> " crlf)
   ) 
(defrule recommend20

  (user-pref(gender female)(category shorts)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name "You can choose one of the following shorts- <for Women>" crlf "i.Blue denim shorts <$17.99> ii.Paper bag shorts <$14.99> iii.Wide cut shorts <$5.99> " crlf)
   ) 

;fatcs for assertion of questions
(deffacts test-facts
(question (ident gender)(type male-female)
   (text "What is your gender? "))
(question (ident category)(type category)
   (text "What type of clothes are you looking for? "))
(question (ident range)(type range)
   (text "What is the price range that you are considering for buying for your clothes?"))
(question (ident occasion)(type occasion)
   (text "What is the occasion for which you want to buy clothes? "))       
(question (ident age)(type age)
   (text "What is your age"))           
  )  

; Finally move the facts to the working memory and run it
	(reset)
	(run)