; Some various Smalltalk Collection classes.
; Author: Tom Almy
; Date:   September 1996
; Revised 2/2011, 3/2011 Renamed from clsdemo.lsp -- it's not just a demo but actually useful!
; Enhanced 9/2015 -- fixed some bugs, added IdentityDictionary
; Enhanced 10/2015 -- got rid of offset to OrderedCollection. Changed SortedCollection sort function argument to keyword.
;                     added :withAll, improved :storeOn, :add takes optional quantity for all classes implementing,
;                     added IdentitySet and made existing Set use equals for comparison. Fixed some bugs, added some
;                     performance enhancements. Tried LinkedList optimized class for stacks and queues but OrderedCollection
;                     turned out to be faster. Added :remove for OrderedCollection (method defined for SortedCOllection by
;                     mistake. Added :asDictionary and :asIdentityDictionary. Added :associations, :doKeys, :mapKeys for Dictionary.
;                     Added add and remove methods for Array (these are slow!). Added :withAll for dictionaries.
;                     Added :clone to Collection.

; I could improve by writing new :do methods instead of relying on :map and then tossing the resulting list.

; NOTE -- you should probably check out EXAMPLE.LSP, TURTLES.LSP, and
; BLOCKS.LSP first as they are somewhat simpler.


#-:common (load "common")
#-:classes (load "classes")	; We'll use these nice macros

(provide "collections")

; We will put everyting in a package to keep it out of the user name space

#+:packages (unless (find-package "COLLECTIONS")
		    (make-package "COLLECTIONS" :use '("XLISP")))

(in-package "COLLECTIONS")

; List the symbols available on the outside -- in this case the class names.
; The message selectors are all in the keyword package so they don't need
; to be exported.

(export '(Collection Set IdentitySet Bag Dictionary IdentityDictionary SequenceableCollection
          Array OrderedCollection SortedCollection Interval))
		     


; Our basic Collection class is "abstract" -- it's just defined to
; subclass into useful types of collections. We'll define two single instance
; variables: "data" contains the collection's data, the format to be
; defined by the subclass, and "cfcn" the compare function used by the class, or nil if none.
; We want this to be able to make copies of a collection. Various subclasses will define any additional
; instance variables.

; The actual collections used in applications will be created from subclasses
; of Collection. This code will implement:

;  Bag -- an unordered collection of objects. Uses eql for comparison to detect duplicates and save space.
;  Set -- like a bag, but no duplicate elements -- uses equal for comparison
;    IdentitySet -- Like set but uses eql for comparison
;  Dictionary -- access elements using symbolic keys (uses equal for key comparison)
;    IdentityDictionary -- Dictionary that uses eql for key comparision.
;  SequenceableCollection -- Abstract class which is subclassed into:
;    Array -- elements have a sequence. Collection has a fixed size. Designed for fast access. Adding/deleting elements is very slow.
;    OrderedCollection -- same as Array but no fixed size, can add/delete from
;         either end.
;      SortedCollection -- An Ordered collection with a colating sequence
;    Interval -- contains a constant sequence

; Methods we have:
; :new --often takes argument that is class specific:
;   Bag  :withAll <list of initial values or collection of initial values>
;   Set  :withAll
;   IdentitySet :withAll
;   Dictionary :withAll
;   IdentityDictionary :withAll
;   Array  :size <number of elements> or :withAll
;   OrderedCollection :withAll
;   SortedCollection  :cmpfcn <compare function> defaults to #'< :withAll
;   Interval start end and optional step (step size defaults to 1)
; :map -- maps a fuction over the data elements (values for a Dictionary). Returned value is a list.
; :mapKeys -- maps a function over the keys in a Dictionary
; :addAll -- adds argument collection or list elements into collection. key-value pairs for Dictionaries.
; :prin1 -- prints a representation of the class object
; :storeOn -- creates an expression that when executed will create a copy of the object
; :at -- fetch element from a SequencableCollection or Dictionary
; :first -- fetch first element from a SequencableCollection
; :last -- fetch last element from a SequencableCollection
; :atPut -- replace element in a SequencableCollection (except Interval). Add or replace element in a Dictionary.
; :add -- adds (one or more instances of) argument to collection. Becomes last element for OrderedCollection.
;         Works for all Collections but Dictionary and Interval.
; :addFirst -- adds argument to start of SequenceableCollections. Does not work for SortedCollection.
; :addLast -- adds argument to end of  SequenceableCollections. Does not work for SortedCollection.
; :remove -- removes element matching argument, first one if multiple occurances. Works with Bag, Set,  Array, SequenceableCollections
; :removeKey -- remove element for key in a Dictionary.
; :removeFirst -- removes first element of  SequenceableCollections.
; :removeLast -- removes last element of SequenceableCollections
; :size -- number of elements
; :isEmpty -- T if size is zero
; :includes -- T if argument is member of collection. Applies to value for Dictionary.
;              If you want to see if a Dictionary includes a key, use :at.
; :species -- returns class similar to objects class to create new objects.
; :do -- same as map but returns nil
; :doKeys -- same as mapKeys but returns nil                                                                                           
; :collect -- same as map but returns a new collection
; :select -- returns a collection of elements for which the predicate function returns non-nil
; :asList -- returns collection elements as a list.
; :asBag -- returns collection elements as a Bag.
; :asSet -- returns collection elements as a Set.
; :asIdentitySet -- returns collection elements as an IdentitySet.
; :asDictionary -- returns collection elements as a Dictionary where keys are integers starting at 0                                                                                           
; :asIdentityDictionary -- returns collection elements as an IdentityDictionary where keys are integers starting at 0                                                                                           
; :asArray -- returns collection elements as an Array.
; :asOrderedCollection -- returns collection elements as an OrderedCollection
; :asSortedCollection -- returns collection elements as a SortedCollection :cmpfcn keyword argument
; :keys -- returns a Set of the keys in a Dictionary
; :values -- returns a Bag of the values in a Dictionary
; :associations -- returns a Set of the associations in a Dictionary
; :clone -- returns a clone of self.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE COLLECTION CLASS                                   ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+:packages (shadow 'data) ;; because it is also in package XLISP
(defclass Collection (data cfcn) nil nil
"Collection   BASE CLASS FOR ALL COLLECTION CLASSES
    This is an abstract class with some utility messages, often
    overridden in subclasses.
***
Implemented subclasses are:    
Bag -- an unordered collection of objects. Uses eql for comparison to
       detect duplicates and save space.
Set -- like a bag, but no duplicate elements -- uses equal for comparison
  IdentitySet -- Like set but uses eql for comparison
Dictionary -- access elements using symbolic keys (uses equal for key comparison)
  IdentityDictionary -- Dictionary that uses eql for key comparision.
SequenceableCollection -- Abstract class which is subclassed into:
  Array -- elements have a sequence. Collection has a fixed size. Designed for fast access.
           Adding/deleting elements is very slow.
  OrderedCollection -- same as Array but no fixed size, can add/delete from
                       either end.
    SortedCollection -- An Ordered collection with a colating sequence
  Interval -- contains a constant sequence
***
Message selectors we have:
:new --often takes argument that is class specific:
  Bag  :withAll <list of initial values or collection of initial values>
  Set  :withAll
  IdentitySet :withAll
  Dictionary :withAll
  IdentityDictionary :withAll
  Array  :size <number of elements> or :withAll
  OrderedCollection :withAll
  SortedCollection  :cmpfcn <compare function> defaults to #'< :withAll
  Interval start end and optional step (step size defaults to 1)
:map -- maps a fuction over the data elements (values for a Dictionary). Returned value is a list.
:mapKeys -- maps a function over the keys in a Dictionary
:addAll -- adds argument collection or list elements into collection. key-value pairs for Dictionaries.
:prin1 -- prints a representation of the class object
:storeOn -- creates an expression that when executed will create a copy of the object
:at -- fetch element from a SequencableCollection or Dictionary
:first -- fetch first element from a SequencableCollection
:last -- fetch last element from a SequencableCollection
:atPut -- replace element in a SequencableCollection (except Interval). Add or replace element in a Dictionary.
:add -- adds (one or more instances of) argument to collection. Becomes last element for OrderedCollection.
        Works for all Collections but Dictionary and Interval.
:addFirst -- adds argument to start of SequenceableCollections. Does not work for SortedCollection.
:addLast -- adds argument to end of  SequenceableCollections. Does not work for SortedCollection.
:remove -- removes element matching argument, first one if multiple occurances. Works with Bag, Set,  Array, SequenceableCollections
:removeKey -- remove element for key in a Dictionary.
:removeFirst -- removes first element of  SequenceableCollections.
:removeLast -- removes last element of SequenceableCollections
:size -- number of elements
:isEmpty -- T if size is zero
:includes -- T if argument is member of collection. Applies to value for Dictionary.
             If you want to see if a Dictionary includes a key, use :at.
:species -- returns class similar to objects class to create new objects.
:do -- same as map but returns nil
:doKeys -- same as mapKeys but returns nil                                                                                           
:collect -- same as map but returns a new collection
:select -- returns a collection of elements for which the predicate function returns non-nil
:asList -- returns collection elements as a list.
:asBag -- returns collection elements as a Bag.
:asSet -- returns collection elements as a Set.
:asIdentitySet -- returns collection elements as an IdentitySet.
:asDictionary -- returns collection elements as a Dictionary where keys are integers starting at 0                                                                                           
:asIdentityDictionary -- returns collection elements as an IdentityDictionary where keys are integers starting at 0                                                                                           
:asArray -- returns collection elements as an Array.
:asOrderedCollection -- returns collection elements as an OrderedCollection
:asSortedCollection -- returns collection elements as a SortedCollection :cmpfcn keyword argument
:keys -- returns a Set of the keys in a Dictionary
:values -- returns a Bag of the values in a Dictionary
:associations -- returns a Set of the associations in a Dictionary
:clone -- returns a clone of self."
          )

; The defclass macro defines a new class, which is bound to the symbol
; "Collection". The macro also defines several default methods for instance
; variable access (:data and :cfcn in this case), and instance initialization (:isnew).

; Unlike Smalltalk, XLISP has no class methods. In Smalltalk you create an
; instance of a class by sending a message to the class. In XLISP, the classes
; are members of class Class, and you create an instance by sending the class
; a message with the selector :new. i.e. (send MyClass :new <xxx>) where xxx
; are 0 or more arbitrary expressions. This executes the :new method in class
; Class which will create an object which is a member of the desired class
; (call it newobj) and then does a (send newobj :isnew <xxx>). The :isnew
; method gets the expressions, which it can then use to customize the
; initiation. So, basically, the :isnew method in a class takes the place of
; the class methods of Smalltalk. Not as functional, but it usually does all
; that is needed. The class Array demonstrates how two instance creation
; methods can be supported via the use of keyword arguments.

;;;;;;;;;
; The following group of methods are "private" methods in that they are not
; intended for use in applications, but just to aid in implementation.

; :notImplemented provides a nice error message for messages that aren't
; handled by our class. It's not really necessary to define these!

(defmethod Collection :notImplemented (msg)
           (error "~s not handled in class ~a"
                  msg
                  (send (send self :class) :pname)))

; Here's the difference:
; >(send x :foo)                                    :notImplemented not USED
; error: no method for this message - :foo 

; >(send x :foo)                                :notImplemented  USED
; error: :foo not handled in class Bag

; :map is a mapcar like mapping function for the collection.
; This version only works when data instance variable is sequence of
; collection elements. We will have to override the method for subclasses
; that maintain their data differently.

(defmethod Collection :map
"Collection :map <fcn>
	    Applies the function to all the elements of the collection,
	    answering a list of the function results.
	    <fcn> The function of one argument"
	   (fcn) (map 'cons fcn data))

; :addAll will add the elements in the argument collection to this
; collection. We'll extend this definition so it works with sequences as
; well. It won't work with Arrays (which are a fixed size), Intervals
; (which are not alterable), or Dictionaries (which require keys).

(defmethod Collection :addAll
"Collection :addAll <arg>
	    All the elements in the argument collection or sequence to
	    this collection
	    <arg>	Either a collection or a sequence."
	   (arg)
           (if (or (listp arg) (arrayp arg) (stringp arg))
               ; Use map when argument is a sequence
               (map nil (lambda (x) (send self :add x)) arg)
               ; Otherwise, send :map to the argument collection
               (send arg :map (lambda (x) (send self :add x))))
           self)

; Override default "isnew" to disallow creating abstract collections.
; There is no reason for any program to create an instance of Collection.

(defmethod Collection :isnew (&rest dummy)
	   (error "Don't create collections of class \"Collection\""))

;;;;;;;;;
; Now we will define some "public" methods for Collection. Most will be
; overriden in a subclass. The rest we will provide with a common default
; functionality.


; :prin1 determines how an object is printed. The default is to print
; the objects class and unique ID. We want to do better than that if the
; collection is small enough to easily display, based on value of *print-length*

(defmethod Collection :prin1
"Collection :prin1 [<stream>]
	    If collection is small enough, print it on stream.
	    <stream>	Output stream, default *standard-output*"
	   (&optional (stream *standard-output*))
           (let ((contents (send self :asList)) ; get collection as a list
                 (cls(send(send self :class):pname))) ; and get our class' name
                (cond ((null contents)
                       (format stream "#<An empty ~a>" cls))
                      ((or (not (typep *print-length* 'integer)) (< (length contents) *print-length*))
                       (format stream "#<~a:~{ ~s~}>" cls contents))
                      (t
                       (format stream "#<~a:~v{ ~s~} ...>" cls *print-length* contents)))))
		       

; :storeon is used to create an expression which, when executed, will create a
; copy of the object. The Default method, part of class Object, won't work
; for classes that override :isnew, and all Collection classes do.

(defmethod Collection :storeon
"Collection :storeon
	    Answers an expression which, when executed, will create a copy of the object"
	   ()
           (if cfcn
               (list 'send
                     (intern (send (send self :class) :pname))
                     :new
                     :cmpfcn
                     cfcn
                     :withAll
                     (list
                      'quote
                      (send self :asList)))
               (list 'send
                     (intern (send (send self :class) :pname))
                     :new
                     :withAll
                     (list
                      'quote
                      (send self :asList)))))

; :clone clones the collection by using :storeon.

(defmethod Collection :clone
"Collection :clone
	    Answers a clone (shallow copy) of the collection."
	   ()
           (eval (send self :storeOn)))                                                                                                                                                                                      
                                                                                           
; :at will fetch an element from a "sequenceable collection"
; Not all collections have the concept of sequencing.

(defmethod Collection :at (arg) (send self :notImplemented :at))

; :atput will store an element into a "sequenceable collection".

(defmethod Collection :atPut (arg1 arg2) (send self :notImplemented :atPut))

; :first will fetch the first element of the collection, where appropriate.
; :last does the same thing but for the last element.

(defmethod Collection :first () (send self :notImplemented :first))
(defmethod Collection :last () (send self :notImplemented :last))

; :add will store (one or more copies of) an element into a collection
; :addFirst will add to the start of a collection. These two are not
; implemented for all classes.

(defmethod Collection :add (arg &optional value)
           (send self :notImplemented :add))

(defmethod Collection :addFirst (arg) 
           (send self :notImplemented :addFirst))

(defmethod Collection :addLast (arg) 
           (send self :notImplemented :addLast))

; Delete the specified, first, or last element

(defmethod Collection :remove (arg)
           (send self :notImplemented :remove))

(defmethod Collection :removeFirst ()
           (send self :notImplemented :removeFirst))

(defmethod Collection :removeLast ()
           (send self :notImplemented :removeLast))

; :size -- Get the size of the the Collection. This will work for
; most subclasses.

(defmethod Collection :size
"Collection :size
	    Answers the number of elements in the collection."
	   () (length data))

; :isEmpty -- Returns T if collection has no elements

(defmethod Collection :isEmpty
"Collection :isEmpty
	    Answers T if collection has no elements, else NIL"
	   () (zerop (send self :size)))

; :includes tells us if a object is a member of the collection
; This version only works when data instance variable is sequence of
; collection elements

(defmethod Collection :includes
"Collection :includes <arg>
	    Answers T if collection contains <arg>, else NIL"
	   (arg)
           (if (position arg data) t nil))

; :species returns the class similar to the current class to create new
; objects

(defmethod Collection :species
"Collection :species
	    Answers the class similar to the current class to create new objects"
	   ()
           (send self :class))


; :do is like :map but returns nothing

(defmethod Collection :do
"Collection :do <fcn>
	    Apply the function to each element of the collection
	    <fcn> A function of one argument."
	   (fcn) (send self :map fcn) nil)

; :collect is like :map, but returns a new collection.
; :select returns a collection of elements for which the predicate function
;    returns non-NIL.
; These are generic enough to work for any of the Collection subclasses
; however in many cases they could be overridden for speed.
; Smalltalk defines these and a number of similar functions.

(defmethod Collection :collect
"Collection :collect <fcn>
	    Answers a new collection from collection elements each applied to a function.
	    <fcn>	Function of one argument"
	   (fcn)
           (let ((result (send self :map fcn)))
                (if cfcn
                    (send (send self :species) :new :cmpfcn cfcn :withAll result)
                    (send (send self :species) :new :withAll result))))

(defmethod Collection :select
"Collection :select <fcn>
	    Answers a new collection from collection elements that meet test predicate.
	    <fcn>	Predicate function of one argument"
	   (fcn)
           (let ((result
                  (mapcan (lambda (x)
                                  (when (funcall fcn x)
                                        (list x)))
                          (send self :asList))))
                (if cfcn
                    (send (send self :species) :new :cmpfcn cfcn :withAll result)
                    (send (send self :species) :new :withAll result))))

; Our final assortment of Collection methods create copies of the object in
; one of several Collection subclasses or as an LISP list.

; :asList will return the collection as a LISP linked list.

(defmethod Collection :asList
"Collection :asList
	    Answers the collection as a list"
	    () (send self :map #'identity))

; :asBag will return the collection as a Bag

(defmethod Collection :asBag
"Collection :asBag
	    Answers the collection as a Bag"
	   ()
           (let ((result (send Bag :new)))
                (send result :addAll self)
                result))

; :asSet will return the collection as a Set

(defmethod Collection :asSet
"Collection :asSet
	    Answers the collection as a Set"
	   ()
           (send Set :new :withAll self))

(defmethod Collection :asIdentitySet
"Collection :asIdentitySet
	    Answers the collection as an IdentitySet"
	   ()
           (set IdentitySet :new :withAll self))                                                                                                                                                                                                                                                                               
                                                                                           
; :asArray will return the collection as an Array

(defmethod Collection :asArray
"Collection :asArray
	    Answers the collection as an Array"
	   ()
	   (send Array :new :withAll self))
		

; :asOrderedCollection will return the collection as an OrderedCollection

(defmethod Collection :asOrderedCollection
"Collection :asOrderedCollection
	    Answers the collection as an OrderedCollection"
	   ()
           (send OrderedCollection :new :withAll self))


; :asSortedCollection will return the collection as a sortedCollection

(defmethod Collection :asSortedCollection
"Collection :asSortedCollection &key <cmpfcn>
	    Answers the collection as a SortedCollection
	    <cmpfcn>  Comparison function, default #'<"
	   (&key (cmpfcn #'<))
           (send SortedCollection :new :cmpfcn cmpfcn :withAll self))

                                                                                           
; :asDictionary will return the collection as a Dictionary where the keys are integer 0 and up
(defmethod Collection :asDictionary
"Collection :asDictionary
	    Answers the collection as a Dictionary where the keys are integers 0 and up"
	   ()
           (let ((n -1)
                 (result (send Dictionary :new)))
                (send self :do (lambda (x) (send result :atPut (incf n) x)))
                result))
                
(defmethod Collection :asIdentityDictionary
"Collection :asIdentityDictionary
	    Answers the collection as an IdenityDictionary where the keys are integers 0 and up"
	   ()
           (let ((n -1)
                 (result (send IdentityDictionary :new)))
                (send self :do (lambda (x) (send result :atPut (incf n) x)))
                result))
                
                
                                                                                                           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE SET CLASS                                          ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Our first collection will be "Set".  Initialization doesn't have to do
; anything since instance variables are initialized to NIL.	   
; We will use "eql" as the equality test for IdentitySet and equal for Set
;

(defclass Set () nil Collection
"Set	COLLECTION CLASS
	A Set is a collection in which all elements are unique using the equal
	comparison. The elements are not ordered."
	  )
(defclass IdentitySet () nil Set
"IdentitySet  COLLECTION CLASS
	      An IdentitySet is a Set where eql is used to test for unique elements."
	  )

(defmethod Set :isnew (&key withAll)
           (when withAll
                 (send self :addAll withAll))
           )


; We will need :add. But we will ignore the count if >1

(defmethod IdentitySet :add
"IdentitySet :add <arg> [<count>]
	     Adds <arg> to collection if it isn't already present.
	     <count> if present must be >= 1 for <arg> to be added."
	   (arg &optional (count 1))
           (when (> count 0)
                 (setq data (adjoin arg data)))
           ; Methods typically return (or "answer" in Smalltalk)
           ; the object, which is bound to "self", if there is
           ; nothing more appropriate.
           self)

(defmethod Set :add
"Set :add <arg> [<count>]
     Adds <arg> to collection if it isn't already present.
     <count> if present must be >= 1 for <arg> to be added."
	   (arg &optional (count 1))
           (when (> count 0)
                 (setq data (adjoin arg data :test #'equal)))
           ; Methods typically return (or "answer" in Smalltalk)
           ; the object, which is bound to "self", if there is
           ; nothing more appropriate.
           self)

; We also need to define :remove

(defmethod IdentitySet :remove
"IdentitySet :remove <arg>
	     Remove element arg from the collection"
	   (arg)
           (let ((pos (position arg data))) ; Find (first) instance
                (when pos ; Delete found element
                      (if (zerop pos)
                          (setq data (cdr data))
                          (setf (cdr (nthcdr (1- pos) data))
                                (nthcdr (1+ pos) data))))
                self))

(defmethod Set :remove
"Set :remove <arg>
     Remove element arg from the collection"
	   (arg)
           (let ((pos (position arg data :test #'equal))) ; Find (first) instance
                (when pos ; Delete found element
                      (if (zerop pos)
                          (setq data (cdr data))
                          (setf (cdr (nthcdr (1- pos) data))
                                (nthcdr (1+ pos) data))))
                self))

; All the other methods inherited from Collection will work fine

; At last we can test out some collections!

; > (setq x (send Set :new))               Create a new set
; #<An empty Set>

; Note that if your system says "#<An empty SET>" that means you have
; *readtable-case* set to :upcase. It's nothing to be concerned about, but
; if you want the output to match, start over with *readtable-case* set to
; :invert.

; > (send x :add 3)                        Add the element "3"
; #<Set: 3>               
; > (send x :add 1)                        Add the element "1"
; #<Set: 1 3>
; > (send x :add 3)                        Add another 3 -- it's ignored!
; #<Set: 1 3>        
; > (send x :addAll '(1 2 3 4 5))          Add five elements
; #<Set: 5 4 2 1 3>

; We see the order has changed! This doesn't matter because these collections
; are defined to have no order.

; > (send x :remove '3)                    Remove element "3"
; #<Set: 5 4 2 1>                  
; > (send x :select #'evenp)               Create a set with even elements of x
; #<Set: 2 4>
; > (send x :collect #'1+)                 Create a set with incremented
;                                          elements of x. Note that :collect returns a set, so it might have fewer elements!
; #<Set: 2 3 5 6>
; > (let ((cnt 0)) (send x :do (lambda (x) (incf cnt x))) cnt)
; 12                                       Summing all the elements in the set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE BAG CLASS                                          ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Our next Collection class will be "Bag" which is an unordered collection
; of objects that we will implement with a hash table. The table value will
; be the number of occurances of the object in the collection.
; It's difficult to calculate the number of elements in a Bag, so we will
; maintain a running total in a new instance variable, size. The defclass
; function will create a :size method for us!
;

; After we've defined this class, we can finally start testing things out.


(defclass Bag (size) nil Collection
"Bag	COLLECTION CLASS
	A Bag is a unordered collection. It is implemented using
	a hash table where the keys are the objects and the values
	are the quantity of that object"
	  )

; Because the data in a Bag will be a hash table instead of a list, we
; need to have "isnew" allocate a hash table.
; The entry equality test will be "eql"

(defmethod Bag :isnew (&key withAll)
           (setf (send self :data) (make-hash-table)
                 (send self :size) 0)
           (when withAll (send self :addAll withAll))
           )


; We could have done this with "(setf data (make-hash-table) size 0)"
; but this technique is more rigorous.


; The method :add will insert one or more copies of an object in the collection
; We need to adjust the size instance variable when we add objects

(defmethod Bag :add
"Bag :add <arg> [<count>]
     Adds <count> copies of <arg> to collection. Count defaults to 1"
	   (arg &optional (count 1))
           (setf (gethash arg data) (+ (gethash arg data 0) count)
                 size (+ size count))
           self  ; Most methods return Self if there isn't anything else
           )     ; that is reasonable

; The method :remove will delete an object from the collection
; We need to adjust the size instance variable when we delete objects

(defmethod Bag :remove
"Bag :remove <arg>
     Remove element arg from the collection"
	   (arg)
           (let ((cnt (gethash arg data)))
                (when cnt ; element found
                      (setq size (1- size))
                      (if (= cnt 1)
                          (remhash arg data) ; delete if count would be 0
                          (setf (gethash arg data) (1- cnt))))
                self
                ))

; We have to override the definition of :includes since data is stored
; differently in a bag than as a linked list.

(defmethod Bag :includes
"Bag :includes <arg>
     Answers T if collection contains <arg>, else NIL"
	   (arg)
           (if (gethash arg data) t nil))

; We have to override the definition of :map since data is stored
; differently in a bag than as a linked list.
; Even though :collect is similar, we don't need to redefine it since
; Collection :collect uses :map to do its work.


(defmethod Bag :map
"Bag :map <fcn>
     Applies the function to all the elements of the collection,
     answering a list of the function results.
     <fcn> The function of one argument"
     (fcn)
	   (if data  ; If in the rare case data isn't set up, we abort
               (let (result)
                    (maphash (lambda (arg count)
                                     (dotimes (i count)
                                              (push (funcall fcn arg) result)))
                             data)
                    (nreverse result))
               nil))


; Now for some Bag examples:


; > (setq y (send Bag :new))                 Create a new bag, y
; #<An empty Bag>
; > (send y :add 3)                          As with set, add 3, 1, 3
; #<Bag: 3>
; > (send y :add 1)
; #<Bag: 3 1>
; > (send y :add 3)
; #<Bag: 3 3 1>                              Now there can be multiple copies!
; > (send y :addAll x)                       Add all the elements of Set x
; #<Bag: 5 4 3 3 2 ...>                      Elipsis means too many to display
; > (send y :asList)                         Use :asList to see entire contents
; (5 4 3 3 2 1 1)
; > (send y :remove 4)
; #<Bag: 5 3 3 2 1 ...>                      Remove still works
; > (send y :select #'oddp)                  Try :select :collect and :do
; #<Bag: 5 3 3 1 1>
; > (send (send y :collect #'1+) :asList)
; (6 4 4 3 2 2)
; > (let ((cnt 0)) (send y :do (lambda (x) (incf cnt x))) cnt)
; 15
; > (send y :asSet)                          Converting a Bag to a Set
; #<Set: 1 2 3 5>                            will delete duplicates


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE DICTIONARY AND IDENITYDICTIONARY                   ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The Dictionary class will be implemented using a hash table like a Bag.
; The hash table will use #'equal for comparisons, allowing string keys
; The IndentityDictionary class is a subclass that uses #'eql for comparisons.
; perhaps we should use eq, but in this case eql will be faster.

(defclass Dictionary () nil Collection
"Dictionary	COLLECTION CLASS
		A Dictionary is a collection of key-value pairs where
		the key is used to index the values. #'equal is used for comparisons."
	  )

(defmethod Dictionary :isnew (&key withAll)
           (setf (send self :data) (make-hash-table :test #'equal))
           (when withAll
                 (send self :addAll withAll)))

(defclass IdentityDictionary () nil Dictionary
"IdentityDictionary	COLLECTION CLASS
		An IdentityDictionary is a collection of key-value pairs where
		the key is used to index the values. #'eql is used for comparisons."
	  )

(defmethod IdentityDictionary :isnew (&key withAll)
           (setf (send self :data) (make-hash-table))
           (when withAll
                 (send self :addAll withAll)))

; Getting the size of a Dictionary is slightly different than the default
	   
(defmethod Dictionary :size
"Dictionary :size
	    Answers the number of elements in the collection."
	   () (hash-table-count data))

; We need to define the :at and :atPut methods. :at will be extended
; to allow a keyword argument "ifAbsent" to supply the return value.
; It's a closure, just like in Smalltalk

(setq gened (gensym))  ; We need a unique symbol

(defmethod Dictionary :at
"Dictionary :at <key> &key <ifAbsent>
	    Answer the value with the given key in the collection. If
	    key doesn't exist answers result of evaluating ifAbsent or nil"
	   (key &key ifAbsent)
           (let ((value (gethash key data '#.gened)))
                (if (eq value '#.gened)
                    (if ifAbsent
                        (funcall ifAbsent)
                        nil)
                    value)))


(defmethod Dictionary :atPut
"Dictionary :atPut <key> <value>
	    Sets the value of element with given key to given value.
	    Answers the collection"
	   (key value)
           (setf (gethash key data) value)
           self)


; :addAll needs to be redefined, and requires a list of key-value pairs.
; This method makes :storeon much simpler.

(defmethod Dictionary :addAll
"Dictionary :addAll <arg>
	    All the elements in the argument collection or list to
	    this dictionary. 
	    <arg>	Either a collection or a list of key-value pairs"
	   (arg)
           (if (listp arg)
               ; Use map when argument is a sequence
               (map nil
                    (lambda (x) (send self :atPut (first x) (second x)))
                    arg)
               ; Otherwise, send :map to the argument collection
               (send arg
                     :map
                     (lambda (x)
                             (send self :atPut (first x) (second x)))))
           self)



; :remove won't work for a Dictionary, since we want to remove key/value
; associations. Thus we have :removeKey, with an optional ifAbsent.

(defmethod Dictionary :removeKey
"Dictionary :removeKey <key> &key <fcn>
	    Removes the key-value pair with key key. If the key
	    is not found, fcn is executed if provided."
	   (key &key ifAbsent)
           (if (eq (gethash key data '#.gened) '#.gened)
               (when ifAbsent (funcall ifabsent))
               (remhash key data))
           self)

(unintern gened) ; We don't need this symbol anymore

; :keys returns a set of the keys

(defmethod Dictionary :keys
"Dictionary :keys
	    Answers a Set containing all the keys in the collection."
	   ()
           (let (list)
                (maphash (lambda (key value) (setq list (cons key list)))
                         data)
                (send Set :new :withAll list)))

; :values returns a bag of the values

(defmethod Dictionary :values
"Dictionary :values
	    Answers a Bag containing all the values in the collection."
	   ()
           (let (list)
                (maphash (lambda (key value) (setq list (cons value list)))
                         data)
                (send Bag :new :withAll list)))

; :associations get the associations as a set
                                                                                           
(defmethod Dictionary :associations
"Dictionary :associations
	    Answers a Set of all the key-value pairs. The pairs are each a list."
	   ()
           (let (contents) ; get collection as a list
                (maphash (lambda (x y)
                                 (setq contents (cons (list x y) contents)))
                         data)
                (send Set :new :withAll contents)))
                                                                                           

; :map is defined to work over the values

(defmethod Dictionary :map
"Dictionary :map <fcn>
	    Applies the function to all the values of the collection,
	    answering a list of the function results.
	    <fcn> The function of one arguement"
	   (fcn)
           (let (list)
                (maphash (lambda (key value)
                                 (setf list (cons (funcall fcn value) list)))
                         data)
                list))

(defmethod Dictionary :mapKeys
"Dictionary :mapKeys <fcn>
	    Applies the function to all the keys of the collection,
	    answering a list of the function results.
	    <fcn> The function of one argument"
	   (fcn)
           (let (list)
                (maphash (lambda (key value)
                                 (setf list (cons (funcall fcn key) list)))
                         data)
                list))
                                                                                                                                              
;; Since we have to write this anyway, lets make it efficient
(defmethod Dictionary :doKeys
"Dictionary :doKeys <fcn>
	    Applies the function to all the keys of the collection,
	    answering NIL.
	    <fcn> The function of one argument"
	   (fcn) 
           (maphash (lambda (key value) (funcall fcn key)) data))                                                                                          
                                                                                                                                                                                      
; We have to override the definition of :includes since data is stored
; differently in a Dictionary than as a linked list.

(defmethod Dictionary :includes
"Dictionary :includes <arg>
	    Answers T if dictionary contains value <arg>, else NIL."
	   (arg)
           (if (position arg (send self :asList)) t nil))

; :collect, :select aren't appropriate

(defmethod Dictionary :collect
"Dictionary :collect not implemented"
	   (arg) 
           (send self :notImplemented :collect))

(defmethod Dictionary :select
"Dictionary :select not implemented"
	   (arg) 
           (send self :notImplemented :select))

; :prin1 needs to be overridden to show both keys and data

(defmethod Dictionary :prin1
"Dictionary :prin1 [<stream>]
	    If collection is small enough, print it on stream.
	    <stream>	Output stream, default *standard-output*"
	   (&optional (stream *standard-output*))
           (let (contents ; get collection as a list
                          ; and get our class' name
                          ; (it might not be "Dictionary")
                          (cls (send (send self :class) :pname))) 
                (maphash (lambda (x y)
                                 (setq contents (cons (list x y) contents)))
                         data)
                (cond ((null contents)
                       (format stream
                               "#<An empty ~a>" cls))
                      ((or (not (typep *print-length* 'integer)) (< (length contents) *print-length*))
                       (format stream
                               "#<~a:~{ ~s~}>" cls contents))
                      (t
                       (format stream
                               "#<~a:~v{ ~s~} ...>" cls *print-length* contents)))))


; A different :storeon is needed as well

(defmethod Dictionary :storeon
"Dictionary :storeon
	    Answers an expression which, when executed, will create a copy of the object"
	   ()
           (let (contents) ; get collection as a list
                (maphash (lambda (x y)
                                 (setq contents (cons (list x y) contents)))
                         data)
                (list 'send
                      (intern (send (send self :class) :pname))
                      :new
                      :withAll
                      (list 'quote contents))))

; Class Dictionary examples

; > (setq z (send Dictionary :new))              Create a new dictionary
; #<An empty Dictionary>
; > (send z :addAll '((a 1) (b 2) (c 3) (d 4)))  Quickly add 4 entries
; #<Dictionary: (a 1) (b 2) (c 3) (d 4)>
; > (send z :at 'b)                              Given a key, returns value
; 2
; > (send z :at 'e :ifAbsent (lambda () "Key Not Found")) Check ":ifAbsent"
; "Key Not Found"
; > (send z :atPut 'b 7)                         :atPut will change value
; #<Dictionary: (a 1) (b 7) (c 3) (d 4)>
; > (send z :atPut 'e 100)                       :atPut will create new entries
; #<Dictionary: (a 1) (b 7) (c 3) (d 4) (e 100)>
; > (send z :asBag)                              Converting to Bag just gives
; #<Bag: 7 100 4 3 1>                            value

											   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE SEQUENCEABLECOLLECTION CLASS                       ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The class SequenceableCollection is, like Collection, an abstract class.
; This is a good thing since who would want to type "SequenceableCollection"
; very often?
;

(defclass SequenceableCollection () nil Collection
	  )

; Some methods can be defined that will work for all subclasses of
; SequenceableCollection. The minimum index value is 0.

(defmethod SequenceableCollection :at
"SequenceableCollection :at <arg>
		       Answer the arg'th element in the collection"
	   (arg) (elt data arg))

(defmethod SequenceableCollection :atPut
"SequenceableCollection :atPut <arg> <value>
			Set the arg'th element in the collection to value
			and answer the the collection"
	   (arg value)
	   (setf (elt data arg) value)
	   self)

(defmethod SequenceableCollection :first
"SequenceableCollection :first
			Answers the first element in the collection"
	   () (send self :at 0))

(defmethod SequenceableCollection :last
"SequenceableCollection :last
			Answers the last element in the collection"
	   () (send self :at (1- (send self :size))))

(defmethod SequenceableCollection :isnew (&rest dummy)
	   (error "Don't create collections of class \"SequenceableCollection\""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE ARRAY CLASS                                        ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The Array class -- implemented using an array.
; Because it has a fixed size, we have to allocate space for it when
; we create it. .
; Either : (send Array :new :size 10) for example, to create an array of 10
; entries or (send Array :new :withAll '(1 2 3 4 5)) for an initialized array.


(defclass Array () nil SequenceableCollection
"Array	COLLECTION CLASS
	An Array is an ordered collection implemented using an array. That
	makes :at and :atPut fast but adding and removing elements is slow."
	  )

(defmethod Array :isnew (&key size withAll)
	   ; Size must be specified when creating array
	   (if size
	       (setf (send self :data) (make-array size))
           (let ((val (if (listp withAll) withAll (send withAll :asList))))
                (setf (send self :data)
                      (make-array (length val) :initial-contents val)))))

; Optimize collect and select

(defmethod Array :collect
"Array :collect <fcn>
       Answers a new collection from collection elements each applied to a function.
       <fcn>	Function of one argument"
           (fcn)
	   (let ((result (send Array :new :size (send self :size))))
		(map-into (send result :data) fcn data)
		result))

(defmethod Array :select
"Collection :select <fcn>
	    Answers a new collection from collection elements that meet test predicate.
	    <fcn>	Predicate function of one argument"
	   (fcn)
	   (let ((result
		  (mapcan (lambda (x)
				  (when (funcall fcn x)
					(list x)))
			  (coerce (send self :data) 'list))))
		(send (send self :class) :new :withAll result)))

; We allow adding and deleting, but these aren't efficient -- should use OrderedCollection if done frequently
(defmethod Array :add
"Array :add <arg> [<count>]
     Adds <count> copies of <arg> to end of collection. Count defaults to 1"
	   (arg &optional (count 1)) ;; Not very efficient!
           (dotimes (i count) (setf data (concatenate 'array data (list arg))))
           self)

(defmethod Array :addLast
"Array :addLast <arg>
       Appends arg at the end of the collection. Same as :add."
	   (arg) (send self :add arg))

(defmethod Array :addFirst
"Array :addFirst <arg>
       Prepends arg to the beginning of the collection."
	   (arg)
           (setf data (concatenate 'array (list arg) data))
           self)

(defmethod Array :removeFirst
"Array :removeFirst
       Removes the first element from the collection. Answers the element."
	   ()
           (unless (zerop (length data))
                   (let ((result (elt data 0)))
                        (setf data (coerce (rest (coerce data 'list)) 'array))
                        result)))

(defmethod Array :removeLast
"Array :removeLast
       Removes the last element from the collection. Answers the element."
	   ()
           (unless (zerop (length data))
                   (let ((result (send self :last)))
                        (setf data (coerce (butlast (coerce data 'list)) 'array))
                        result)))                                                                                                                                                                                      
                                                                                           
(defmethod Array :remove
"Array :remove <arg>
       Remove first element matching arg from the collection"
	   (arg &aux (seek t))
           (setf data (coerce (mapcan (lambda (x) (if (and seek (equal x arg))
                                                      (prog2 (setf seek nil) nil)
                                                      (list x)))
                                      (coerce data 'list))
                              'array))
           self)
                                                                                                                 

; Test of the Array class:

; > (setq a (send x :asArray))              Make Array from Set x
; #<Array: 5 4 2 1>
; > (send a :atPut 1 10)                    Change an element
; #<Array: 5 10 2 1>
; > (send a :select #'evenp)                Get array of even elements
; #<Array: 10 2>
; > (send a :collect #'1+)                  Make array with values 1 larger
; #<Array: 6 11 3 2>             


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE ORDEREDCOLLECTION CLASS                            ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The OrderedCollection class uses linked lists and doesn't have the
; allocation problems of Array.


(defclass OrderedCollection () nil SequenceableCollection
"OrderedCollection	COLLECTION CLASS
			An OrderedCollection is implemented using linked
			lists. Accessing elements is slower than Array
			but adding and removing elements is faster."
	  )

(defmethod OrderedCollection :isNew (&key withAll)
           (when withAll (send self :addAll withAll)))

; :at, :atPut, :first, and :last need revision

(defmethod OrderedCollection :at
"OrderedCollection :at <arg>
		   Answer the arg'th element in the collection"
	   (arg) (elt data arg ))

(defmethod OrderedCollection :atPut
"OrderedCollection :atPut <arg> <value>
		   Set the arg'th element in the collection to value
		   and answer the the collection"
	   (arg value)
	   (setf (elt data arg) value)
	   self)

(defmethod OrderedCollection :first
"OrderedCollection :first
		   Answers the first element in the collection"
	   () (car data))

(defmethod OrderedCollection :last
"OrderedCollection :last
			Answers the last element in the collection"
	   () (car (last data)))

(defmethod OrderedCollection :isEmpty
"OrderedCollection :isEmpty
		   Answers T if collection has no elements, else NIL"
	   () (null data))  ;; Slightly faster

; We need to implement add and remove for both ends
; :add will be equivalent to :addLast

(defmethod OrderedCollection :add
"OrderedCollection :add <arg> [<count>]
     Adds <count> copies of <arg> to collection. Count defaults to 1"
	   (arg &optional (count 1))
	   (dotimes (i count) (setq data (nconc data (list arg))))
	   self)

(defmethod OrderedCollection :addlast
"OrderedCollection :addLast <arg>
		   Appends arg at the end of the collection. Same as :add."
       (arg) (send self :add arg))

(defmethod OrderedCollection :addFirst
"OrderedCOllection :addFirst <arg>
		   Prepends arg to the beginning of the collection."
	   (arg)
	   (setq data (cons arg data))
	   self)

(defmethod OrderedCollection :removeFirst
"OrderedCollection :removeFirst
       Removes the first element from the collection. Answers the element."
	   ()
	   (unless (null data)
		   (prog1 (car data)
			  (setq data (cdr data)))))

(defmethod OrderedCollection :removeLast
"Array :removeLast
       Removes the last element from the collection. Answers the element."
	   ()
           (unless (null data)
                   (prog1 (car (last data))
                          (setq data (nbutlast data)))))


(defmethod OrderedCollection :remove
"OrderedCollection :remove <arg>
       Remove first element matching arg from the collection"
	   (arg)
	   (let ((pos (position arg data :test #'equal))) ; Find (first) instance
		(when pos ; Delete found element
		      (if (zerop pos)
			  (setq data (cdr data))
			  (setf (cdr (nthcdr (1- pos) data))
				(nthcdr (1+ pos) data))))
		self))


; Example of use of OrderedCollection:


; > (setq  c (send a :asOrderedCollection))   Make one from Array a
; #<OrderedCollection: 5 10 2 1>
; > (send c :at 1)                            Value at index 1 is 10
; 10
; > (send c :addFirst 7)                      Add to front of collection
; #<OrderedCollection: 7 5 10 2 1>
; > (send c :at 1)                            Index 1 is now different
; 5
; > (send c :removeLast)                      Remove from either end
; 1
; > (send c :last)                            Last element is now 2
; 2
; > (send c :asArray)                         Convert back to an array
; #<Array: 7 5 10 2>                        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE SORTEDCOLLECTION CLASS                             ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The SortedCollection class requires a sort function. The collection gets
; re-sorted whenever a new element is added. This is a subclass of
; OrderedCollection.


(defclass SortedCollection () nil OrderedCollection
"SortedCollection COLLECTION CLASS
		  A SortedCollection is an OrderedCollection that is
		  maintained in a sorted order based on a comparison
		  function, by default #'<. A different comparison
		  function can be specified when the collection is
		  created by using the keyword argument :cmpfcn."
	  )

(defmethod SortedCollection :isnew (&key (cmpfcn #'<) withAll)	
	   (setq cfcn cmpfcn)
	   (send-super :isnew :withAll withAll))

(defmethod SortedCollection :selfSort ()
	   ; "private" method that sorts the list
	   (setq data (sort data cfcn))
	   self)

(defmethod SortedCollection :add
"SortedCollection :add <arg> [<count>]
		  Adds <count> copies of <arg> to collection. Count defaults to 1"
	   (arg &optional (count 1))
	   (send-super :add arg count)
	   (send self :selfSort))

; (defmethod SortedCollection :removeFirst ()
; 	   (unless (zerop (length data))
; 		   (prog1 (car data) (setq data (cdr data)))))

;; :removefirst and :removeLast are inherited

; Don't allow addFirst, addLast, atPut

(defmethod SortedCollection :addFirst
"SortedCollection :addFirst is not allowed."
	   (arg) 
	   (send self :notImplemented :addFirst))

(defmethod SortedCollection :addLast
"SortedCollection :addLast is not allowed."
	   (arg) 
	   (send self :notImplemented :addLast))


(defmethod SortedCollection :atPut
"SortedCollection :atPut is not allowed."
	   (arg1 arg2)
	   (send self :notImplemented :atPut))

; We need a way to remove elements from a Sorted Collection.
; :remove (specifying the element) will do just fine.


; Let's see how the SortedCollection works:

; > (setq s (send c :asSortedCollection))    Sorted when it is created
; #<SortedCollection: 2 5 7 10>
; > (send s :add 8)                          :add puts new element in order
; #<SortedCollection: 2 5 7 8 10>
; > (send s :asSortedCollection :sortfcn #'>)         New collection with order reversed
; #<SortedCollection: 10 8 7 5 2>
; > (send (send (send Set :new) :addAll '(5 3 8 2 5 4 8)) :asSortedCollection)
; #<SortedCollection: 2 3 4 5 8>    Eliminate duplicates and sort
; > (send * :asList)
; (2 3 4 5 8)        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE INTERVAL CLASS                                     ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; The Interval class is considerably different than the others in that it has
; no "data" but calculates each elements value. We'll use three new
; instance variables -- start end and step. The :new function will take two
; or three arguments to specify start, end and step, with the step size
; defaulting to 1 not defined. We will set the data instance
; variable to T if the range is valid, and to NIL if not (no elements)


(defclass Interval (start end step) nil SequenceableCollection
"Interval	COLLECTION CLASS
		An Interval is a collection whose elements are calculated
		to be an interval. :new takes three argument:
		start - the value of the first element
		end -- the upper bound of the value of the last element,
		       if step < 0 then the lower bound of the value
		step -- optional (default 1) the step size between elements
			this may be negative."
          )

(defmethod Interval :isnew (arg1 arg2 &optional arg3)
           (if arg3
               (setq data (or (and (<= arg1 arg2)(> arg3 0))
                              (and (>= arg1 arg2)(< arg3 0))))
               (setq arg3 1 
                     data (<= arg1 arg2)))
           (setq start arg1 end arg2 step arg3)
           ; Correct End value if necessary
           (unless (zerop (rem (- end start) step))
                   (setq end (- end (rem (- end start) step))))
           )

; :at calculates value. We won't check for out of range.

(defmethod Interval :at
"Interval :at <arg>
          Answer the arg'th element in the collection. No range checking is performed"
           (arg) (+ start (* step arg)))

; :atPut isn't allowed

(defmethod Interval :atPut (arg1 arg2) (send self :notImplemented :atPut))

; :size returns calculated size

(defmethod Interval :size
"Interval :size
          Answers the number of elements in the collection."
           () (if data (1+ (truncate (- end start) step)) 0))

; :includes must be calcuated

(defmethod Interval :includes
"Interval :includes <arg>
          Answers T if interval contains <arg>, else NIL"
           (arg)
           (cond
            ((null data) nil)
            ((> step 0) (and (>= arg start)
                             (<= arg end)
                             (zerop (rem (- arg start) step))))
            (t          (and (<= arg start)
                             (>= arg end)
                             (zerop (rem (- arg start) step))))))

; While Collection bases :asList on :map, we want to base :map on
; :asList

(defmethod Interval :map
"Interval :map <fcn>
          Applies the function to all the elements of the collection,
          answering a list of the function results.
          <fcn> The function of one arguement"
           (fcn) (mapcar fcn (send self :asList)))

(defmethod Interval :asList
"Interval :asList
          Answers the collection as a list"
           ()
           (let ((result nil))
                (when data
                      (dotimes (i (send self :size))
                               (setq result (cons (+ start (* i step))
                                                  result))))
                (nreverse result)))

; Since :do is used often with an Interval, and since the default method
; would create a list of values, it makes sense to reimplement :do
; here as an Interval method. 

(defmethod Interval :do
"Collection :do <fcn>
            Apply the function to each element of the collection
            <fcn> A function of one argument."
           (fcn)
           (when data
                 (dotimes (i (send self :size))
                          (funcall fcn (+ start (* i step)))))
           nil) 

; :collect, :select will work because we will redefine :species to
; create an OrderedCollection rather than an Interval

(defmethod Interval :species
"Interval :species
          Answers the class similar to the current class to create new objects, OrderedCollection"
           () OrderedCollection)

; Override printing methods 

(defmethod Interval :prin1
"Interval :prin1 [<stream>]
          If collection is small enough, print it on stream.
          <stream>      Output stream, default *standard-output*"
           (&optional (stream *standard-output*))
           (format stream
                   "#<~a from ~s to ~s by ~s>"
                   (send (send self :class) :pname)
                   start end step))

; Override :storeon -- this one becomes really easy

(defmethod Interval :storeon
"Interval :storeon
          Answers an expression which, when executed, will create a copy of the object"
           ()
           (list 'send 'Interval :new start end step))


; A few examples of the use of the Interval class:

; > (setq i (send Interval :new 2 10 2))        Make an interval, i
; #<Interval from 2 to 10 by 2>
; > (send i :do (lambda (x) (format t "~s " x)))  Demonstrate :do
; 2 4 6 8 10
; nil
; > (send i :at 3)                              Check operation of :at
; 8                  
; > (send i :size)                              Size of interval
; 5
; > (send i :asList)                            Convert to a list
; (2 4 6 8 10)
; > (send i :asSortedCollection #'>)            Convert to a SortedCollection
; #<SortedCollection: 10 8 6 4 2>               sequence changes!


(in-package "USER")
(use-package "COLLECTIONS")
(push :collections *features*)
