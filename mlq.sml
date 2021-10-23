(*Multi-Level Priority Queue in ML*)


type 'a mlqueue = (int * int * 'a) list

signature MLQPARAM = sig
       type element;
       val max : int;
end;

structure myQueue =
struct
  type element = int;
  val max = 2;
end;


functor MakeQ (param : MLQPARAM):
  sig
    type 'a mlqueue;
    exception Overflow;
    exception Empty;
    exception LevelNoExist;
    exception NotFound;
    val maxlevel : int;
    val new: param.element mlqueue;
    val enqueue : param.element mlqueue -> int -> int -> param.element -> param.element mlqueue;
    val dequeue : param.element mlqueue -> param.element * param.element mlqueue;
    val move : (param.element -> bool) -> param.element mlqueue -> param.element mlqueue;
    val atlevel : param.element mlqueue -> int -> (int * param.element) list;
    val lookup : (param.element -> bool) -> param.element mlqueue -> int * int;
    val isempty : param.element mlqueue -> bool;
    val flatten : param.element mlqueue -> param.element list;
    end = 
  struct
    open param;
    exception Overflow;
    exception Empty;
    exception LevelNoExist;
    exception NotFound;
    val maxlevel = param.max;
    val new = [];

    type 'a mlqueue = (int * int * 'a) list;
  
  fun enqueue [] l p e = 
    if(l>maxlevel) then raise LevelNoExist
    else if (l<0) then raise LevelNoExist 
    else [(l, p, e)]
  | enqueue ((ll, pp, ee)::ls) l p e = 
    if(l>maxlevel) then raise LevelNoExist
     else if (l<0) then raise LevelNoExist 
    else if (ll < l) then (ll, pp, ee)::(enqueue ls l p e)
    else if (ll > l) then (l, p, e)::(ll, pp, ee)::ls
    else if (pp <= p) then (ll, pp, ee)::(enqueue ls l p e)
    else (l, p, e)::(ll, pp, ee)::ls;

  fun dequeue [] = raise Empty
  | dequeue ((lvl,pr,item)::ls) = (item, ls);



  fun flatten [] = [] 
  | flatten ((ll, pp, ee)::ls) = (ee::(flatten ls));



  fun isempty [] = true
  | isempty (l::ls) = false;

  fun lookup pred [] = raise NotFound
  | lookup pred ((ll, pp, ee)::ls) = 
    if ((pred ee) = true) then (ll, pp)
    else (lookup pred ls);

  
  fun atlevel [] n = [] 
  | atlevel ((ll, pp, ee)::ls) n = 
    if (ll<n) then (atlevel ls n)
    else if (ll = n) then (pp, ee)::(atlevel ls n)
    else [];

  fun move pred q = 
  let val x = 
    foldl (fn ((ll, pp, ee), b) =>  
    if ((pred ee) = true andalso ll<maxlevel)
    then b
    else b@[(ll, pp, ee)]) [] q
  in
    foldl (fn ((ll, pp, ee), b) =>  
    if ((pred ee) = true andalso ll<maxlevel)
    then (enqueue b (ll+1) pp ee)
    else b) x q
  end;

end;

structure maxLevel2PQueue = MakeQ (myQueue);

val q = maxLevel2PQueue.new;
val q = maxLevel2PQueue.enqueue q 1 1 2;
val q = maxLevel2PQueue.enqueue q 0 0 3;
val q = maxLevel2PQueue.enqueue q 2 0 5;
val q = maxLevel2PQueue.enqueue q 2 2 1;
val q = maxLevel2PQueue.enqueue q 1 0 4;
val q = maxLevel2PQueue.enqueue q 2 1 6;

val q = maxLevel2PQueue.enqueue q 3 1 6 handle LevelNoExist => q;

fun f x = x>3;

maxLevel2PQueue.move f q;

val (item, q) = maxLevel2PQueue.dequeue q;

val (item, q) = maxLevel2PQueue.dequeue q;

maxLevel2PQueue.atlevel q 1;

fun g x = x<5;

maxLevel2PQueue.lookup g q;



