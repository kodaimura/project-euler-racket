#lang racket

(require racket/string)

;; 1
;; n未満の3と5の倍数の和

;(define sum_multiples_under
;  (lambda (n)
;    (apply + (filter (lambda (x)
;              (or (zero? (modulo x 3))
;                  (zero? (modulo x 5)))) (range 1 n)))))

;(sum_multiples_under 1000)

;; 3,5以外にも無数の倍数に対応できるように。
;; 例えば 2, 3, 5, 7 の倍数とか
;lsの中にnの倍数があるか?
(define multiple?-or
  (lambda (n ls)
    (cond
      ((null? ls) #f)
      ((zero? (modulo n (car ls))) #t)
      (else (multiple?-or n (cdr ls))))))

(define m_u-aux
  (lambda (n ls l)
    (cond
      ((= n 0) l)
      ((multiple?-or n ls) (m_u-aux (- n 1) ls (cons n l)))
      (else (m_u-aux (- n 1) ls l)))))

;n未満のx(何個でも)の倍数
(define multiples_under
  (lambda (n . xs)
    (m_u-aux (- n 1) xs '())))

(define sum_multiples_of_3or5
  (lambda (n)
    (apply + (multiples_under n 3 5))))

;(sum_multiples_of_3or5 10)
;(sum_multiples_of_3or5 1000)

;; 2
;; 400万以下のフィボナッチ数列の偶数の和

(define fi_u-aux
  (lambda (n l)
    (cond
      ((>= (+ (car l) (cadr l)) n) l)
      (else
       (fi_u-aux n (cons (+ (car l) (cadr l)) l))))))

;n以下のフィボナッチ数列
(define fibonacci_under
  (lambda (n)
    (fi_u-aux n '(2 1))))

(define sum_fibonacci_even
  (lambda (n)
    (apply + (filter even? (fibonacci_under n)))))

;(sum_fibonacci_even 4000000)

;; 3
;; 600851475143の素数の最大値

(define prime?-aux
  (lambda (x n)
    (cond
      ((> (* n n) x) #t)
      ((zero? (modulo x n)) #f)
      (else
       (prime?-aux x (+ n 2))))))

(define prime?
  (lambda (x)
    (cond
      ((<= x 1) #f)
      ((= x 2) #t)
      ((even? x) #f)
      (else (prime?-aux x 3)))))

(define prime_factor-aux
  (lambda (x n l)
    (cond
      ((prime? x) (cons x l))
      ((and (prime? n)
            (zero? (modulo x n)))
       (prime_factor-aux (/ x n) n (cons n l)))
      (else
       (prime_factor-aux x (+ n 1) l)))))

(define prime_factor
  (lambda (x)
    (prime_factor-aux x 2 '())))

;(prime_factor 13195)
;(time (car (prime_factor 600851475143)))

;; 4
;; 回文数字? のなかで二桁の数字の掛け算で表せる最大は9009(91*99)
;; 三桁だと?

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((not (eq? (car l1) (car l2))) #f)
      (else
       (eqlist? (cdr l1) (cdr l2))))))

;何桁か?
(define digit
  (lambda (n)
    (length (string->list (number->string n)))))

;回文数字かどうか
(define palindromic_number?
  (lambda (n)
    (let ((l (string->list (number->string n))))
      (eqlist? l (reverse l)))))

;1 -> 1  2->10  3->100
(define digit->1~0
  (lambda (digit)
    (cond
      ((= digit 1) 1)
      (else (* 10 (digit->1~0 (- digit 1)))))))

;digit桁 * digit桁で表せるmax_palindrome_number
(define m_p_n-aux
  (lambda (n x d ret)
    (cond
      ((not (palindromic_number? n)) (m_p_n-aux (+ n 1) (digit->1~0 d) d ret))
      ((<= n (* x x)) (m_p_n-aux (+ n 1) (digit->1~0 d) d ret))
      ((and (zero? (modulo n x)) (= (digit x) (digit (/ n x))))
       (cond
         ((= d (digit x)) (m_p_n-aux (+ n 1) (digit->1~0 d) d n))
         ((< d (digit x)) ret)))
      (else
        (m_p_n-aux n (+ x 1) d ret)))))

(define max_palindrome_number
  (lambda (digit)
    (m_p_n-aux 10 (digit->1~0 digit) digit 0)))

;(max_palindrome_number 2)
;(max_palindrome_number 3)

;; 5
;; 1~20の全ての数字で割りきることができる数字の最小値

; (prime_factor 20) -> '(5 2 2) 定義済み


(define **
  (lambda (x a)
    (define (aux ret x a)
      (if (zero? a) ret
          (aux (* ret x) x (- a 1))))
    (aux 1 x a)))

; ls:'(a a a b c c d) set:'(a b c d e) -> '(3 1 2 1 0)
(define count_each
  (lambda (ls set)
    (cond
      ((null? set) '())
      (else
       (cons (count (lambda (n) (eq? n (car set))) ls)
             (count_each ls (cdr set)))))))

;(primes_under 20) -> '(2 3 5 7 11 13 17 19)
(define primes_under
  (lambda (n)
    (filter prime? (range 2 (+ n 1)))))

(define d_by-aux
  (lambda (x n ls)
    (cond
      ((= x (+ 1 n)) (apply * (map ** (primes_under n) ls)))
      (else
       (d_by-aux (+ x 1) n
                 (map +
                      ls
                     (map (lambda (a) (if (< a 0) 0 a))
                          (map -
                               (count_each (prime_factor x) (primes_under n))
                               ls))))))))
            
(define divisible_by_1~n
  (lambda (n)
    (d_by-aux 2 n (map (lambda (a) 0) (primes_under n)))))


;(divisible_by_1~n 10)
;(time (divisible_by_1~n 20))

;; 6
;; 二乗の和 - 和のニ乗 (1~nの)

(define sum_of_square
  (lambda (n)
    (apply + (map (lambda (a) (* a a)) (range 1 (+ n 1))))))

(define square_of_sum
  (lambda (n)
    (** (apply + (range 1 (+ n 1))) 2)))

(define sqsu-susq
  (lambda (n)
    (- (square_of_sum n) (sum_of_square n))))

;(sqsu-susq 100)

;; 7
;; n番目の素数は

(define prime_no-aux
  (lambda (x c n)
    (cond
      ((prime? x)
       (cond
         ((= n c) x)
         (else (prime_no-aux (+ x 1) (+ c 1) n))))
      (else
       (prime_no-aux (+ x 1) c n)))))

(define prime_no
  (lambda (n)
    (prime_no-aux 2 1 n)))

;(prime_no 6)
;(prime_no 10001)

;; 8
;; 連続するn桁の数字を掛け合わせて最大となるn桁を見つける

(define maxrow-aux
  (lambda (ls n max)
    (cond
      ((< (length ls) n) max)
      ((< max (apply * (take ls n)))
       (maxrow-aux (cdr ls) n (apply * (take ls n))))
      (else
       (maxrow-aux (cdr ls) n max)))))

(define maxrow_value
  (lambda (strnum n)
    (maxrow-aux
      (remove* '(-38) (map (lambda (a) (- (char->integer a) 48))
           (string->list strnum))) n 0)))

(maxrow_value "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450" 13)

;; 9
;; a^2+b^2=c^2 , a+b+c=1000を満たす a,b,c
(define p_t-aux
  (lambda (a b c)
    (cond
      ((= (* c c) (+ (* a a) (* b b))) (list a b c))
      ((<= c b) (p_t-aux (+ a 1) (+ a 2) (- 1000 (+ a a 3))))
      (else
       (p_t-aux a (+ b 1) (- 1000 (+ a b 1)))))))

(define pythagorean_triplet
  (lambda ()
    (p_t-aux 1 2 997)))

;(pythagorean_triplet)

;; 10
;; 2000000未満の素数の和
(define sum_primes_below
  (lambda (n)
    (apply + (primes_under (- n 1)))))
;(time (sum_primes_below 2000000))

#|
(define sum_primes
  (lambda (n)
    (define aux
      (lambda (ret x n)
        (cond
          ((> x n) ret)
          ((prime? x) 
           (aux (+ ret x) (+ x 2) n))
          (else
           (aux ret (+ x 2) n)))))
    (aux 2 3 n)))

(time (sum_primes 2000000))
|#
;; 11
;; やけくそになってしまった
(define vec-ref
  (lambda (v x y)
    (vector-ref (vector-ref v y) x)))

(define column->row
  (lambda (lls)
    (cond
      ((null? (car lls)) '())
      (else (cons (map car lls) (column->row (map cdr lls)))))))

(define diagonal->row-aux
  (lambda (v n m l grid)
    (cond
      ((= n m) (cons (cons (vec-ref v 1 n) l) (diagonal->row-aux v (+ n 1) 1 '() grid)))
      ((> n grid)
       (cond
         ((= n (- (* 2 grid) 1)) (list (list (vec-ref v grid grid))))
         ((= (- (* 2 grid) n) m)
          (cons (cons (vec-ref v (+ 1 (- n grid)) grid) l)
                (diagonal->row-aux v (+ n 1) 1 '() grid)))
         (else (diagonal->row-aux v n (+ m 1)
                                  (cons (vec-ref v (+ 1 (- grid m)) (+ m (- n grid))) l) grid))))
      (else (diagonal->row-aux v n (+ m 1) (cons (vec-ref v (+ (- n m) 1)  m) l) grid)))))

(define diagonal->row
  (lambda (lls)
    (let ((len (length (car lls))))
      (diagonal->row-aux
       (list->vector
        (map list->vector
             (cons (range len) (map (lambda (l) (cons 0 l)) lls)))) 1 1 '() len))))

(define q11-aux
  (lambda (ls max)
    (cond
      ((< (length ls) 4) max)
      ((< max (apply * (take ls 4))) (q11-aux (cdr ls) (apply * (take ls 4))))
      (else (q11-aux (cdr ls) max)))))

(define q11
  (lambda (str)
    (let ((ls (make20*20 str)))
      (apply max (map (lambda (l) (q11-aux l 0))
                      (append ls
                              (column->row ls)
                              (diagonal->row ls)
                              (diagonal->row (map reverse ls))))))))

(define make20*20
  (lambda (str)
    (define aux
      (lambda (ls c l ret)
        (cond
          ((null? ls) (append ret (list l)))
          ((= c 20) (aux ls 0 '() (append ret (list l))))
          (else (aux (cdr ls) (+ c 1) (append l (list (car ls))) ret)))))
    (map (lambda (l) (map string->number l)) (aux (string-split str) 0 '() '()))))

(q11 "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

;; 12
;; 500個以上の除数を持つ最初の三角数
;(define num_of_divisors
;  (lambda (n)
;    (if (even? n)
;        (+ 2 (count (lambda (a) (zero? (modulo n a)))
;                             (range 1 (/ n 2))))
;        (+ 1 (count (lambda (a) (zero? (modulo n a)))
;                             (range 1 (/ (- n 1) 2)))))))

(define num_of_divisors
  (lambda (x n ret)
    (cond
      ((< x (* n n)) ret)
      ((zero? (modulo x n))
       (if (= n (/ x n))
           (+ ret 1)
           (num_of_divisors x (+ n 1) (+ ret 2))))
      (else (num_of_divisors x (+ n 1)  ret)))))

(define q12
  (lambda (num)
    (define q12-aux
      (lambda (x n num)
        (cond
          ((<= num (num_of_divisors x 1 0)) x)
          (else (q12-aux (+ x n) (+ n 1) num)))))
    (q12-aux 1 2 num)))
;(q12 5)
;(time (q12 500))

;; 13
;; 50桁の数字100個の和の上位10桁
(define nums "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

(define lines->list
  (lambda (lines)
    (define aux
      (lambda (ls l ret)
        (cond
          ((null? ls) (append ret (list (apply string l))))
          ((eq? (car ls) #\newline)
           (aux (cdr ls) '() (append ret (list (apply string l)))))
          (else (aux (cdr ls) (append l (list (car ls))) ret)))))
    (aux (string->list lines) '() '())))

(define q13
  (lambda (lines)
    (substring (number->string (apply + (map string->number (lines->list lines)))) 0 10)))

;(q13 nums)

;; 14
;; 1000000以下の数字でコラッツ列が最長になる値
(define collatz_chain
  (lambda (x)
    (define aux
      (lambda (x count)
        (cond
          ((= x 1) (+ 1 count))
          ((even? x) (aux (/ x 2) (+ count 1)))
          (else (aux (+ 1 (* x 3)) (+ count 1))))))
    (aux x 0)))

(define m_c_c-aux
  (lambda (x max ret limit)
    (if (< limit x)
        ret
        (let ((chain (collatz_chain x)))
         (cond
           ((< max chain) (m_c_c-aux (+ x 1) chain x limit))
           (else (m_c_c-aux (+ x 1) max ret limit)))))))

(define max_collatz_chain
  (lambda (limit)
    (m_c_c-aux 1 0 1 limit)))

;(time (max_collatz_chain 1000000))   ;遅い cpu time: 3964 real time: 3973 gc time: 0

;; 15
;; 20×20のマスで左上から右下までの経路

(define combination
  (lambda (n m)
    (/ (apply * (range n (- n m) -1))
       (apply * (range 1 (+ m 1))))))

(define p15
  (lambda (grid)
    (combination (* 2 grid) grid)))

;(p15 2)
;(p15 20)

;; 16
;; 2^15 = 32768 -> 3+2+7+6+8=26 ならば　2^1000は?
;累乗(**) はp5で定義済み

; 123 -> '(1 2 3)
(define integer->list
  (lambda (n)
    (map (lambda (char) (- (char->integer char) 48))
                  (string->list (number->string n)))))
; 123 -> 1+2+3 = 6
(define sum_digits
  (lambda (n)
    (apply + (integer->list n))))

(define p16
  (lambda (x index)
    (sum_digits (** x index))))

;(p16 2 15)
;(p16 2 1000)

;; 17
(define hash17
  (hash 0 "" 1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six"
  7 "seven" 8 "eight" 9 "nine" 10 "ten" 11 "eleven"
  12 "twelve" 13 "thirteen" 14 "fourteen" 15 "fifteen"
  16 "sixteen" 17 "seventeen" 18 "eighteen" 19 "nineteen"
  20 "twenty" 30 "thirty" 40 "forty" 50 "fifty" 60 "sixty"
  70 "seventy" 80 "eighty" 90 "ninety" 100 "hundred" 1000 "onethousand"))
;sousand->onesousand にした。

(define p17-aux
  (lambda (n)
    (cond
      ((= n 0) -3)　;and分
      ((or (<= n 20) (= n 1000)) (string-length (hash-ref hash17 n)))
      ((<= n 99) (let ((tens_place (quotient n 10)))
                   (+ (string-length (hash-ref hash17 (* 10 tens_place)))
                      (string-length (hash-ref hash17 (- n (* 10 tens_place)))))))
      (else
       (let ((hundreds_place (quotient n 100)))
         (+ (string-length (hash-ref hash17 hundreds_place))
            (string-length (hash-ref hash17 100))
            3  ;and分
            (+ (p17-aux (- n (* 100 hundreds_place))))))))))
                 
(define p17
  (lambda (n)
    (apply + (map p17-aux (range 1 (+ n 1))))))
;(p17 5)
;(p17 1000)

;; 18
;; 三角形の経路で最大になる経路の和
(define p18-aux
  (lambda (x l)
    (cond
      ((null? (cdr l)) x)
      (else
       (max (+ x (p18-aux (caadr l) (map (lambda (ls) (drop-right ls 1)) (cdr l))))
            (+ x (p18-aux (cadadr l) (map cdr (cdr l)))))))))

(define p18
  (lambda (triangle_lines)
    (let ((lls (map (lambda (ls) (map string->number ls))
                    (map (lambda (s) (string-split s))
                         (lines->list triangle_lines)))))
      (p18-aux (caar lls) lls))))
#|
(p18
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")
|#

;; 19
;; 1901-1-1 から 2000-12-31 までで、月はじめが日曜日なのは何日?
(define getdays
  (lambda (y m)
    (cond
      ((or (= m 4) (= m 6) (= m 9) (= m 11)) 30)
      ((= m 2) (cond
                 ((zero? (modulo y 400)) 29)
                 ((zero? (modulo y 100)) 28)
                 ((zero? (modulo y 4)) 29)
                 (else 28)))
      (else 31))))

(define p19-aux
  (lambda (y m days ret)
    (let ((d (getdays y m)))
      (cond
        ((and (= y 2001) (= m 1)) ret)
        ((= m 12)
         (if (zero? (modulo (+ days d) 7))
             (p19-aux (+ y 1) 1 (+ days d) (+ ret 1))
             (p19-aux (+ y 1) 1 (+ days d) ret)))
      ((zero? (modulo (+ days d) 7))
       (p19-aux y (+ m 1) (+ days d) (+ ret 1)))
      (else
       (p19-aux y (+ m 1) (+ days d) ret))))))

(define p19
  (lambda ()
    (p19-aux 1901 1 366 0)))

;(p19)

;; 20
;; 10! = 3628800 -> 3+6+2+8+8+0+0=27 とすると 100!は?

(define factorial
  (lambda (n)
    (apply * (range 1 (+ n 1)))))

; sum_digitsは　p16で定義済み (sum_digits 3628800) -> 27
(define p20
  (lambda (n)
    (sum_digits (factorial n))))

;(p20 10)
;(p20 100)

;; 21
;; amicable pair

;xの約数大きい順
(define divisors
  (lambda (x)
    (define aux
      (lambda (x n ret)
        (cond
          ((< x (* n n)) ret)
          ((zero? (modulo x n))
           (if (= n (/ x n))
               (cons n ret)
               (aux x (+ n 1) (append (list n (/ x n))ret))))
          (else (aux x (+ n 1)  ret)))))
    (sort (aux x 2 (list 1 x)) >)))

; (amicable_pair 220) ->'(220 284)
; (amicable_pair 221) ->'()
(define amicable_pair
  (lambda (a)
    (let ((b (apply + (cdr (divisors a)))))
      (if (and (not (= a b))
               (= a (apply + (cdr (divisors b)))))
          (list a b)
          '()))))

(define p21
  (lambda (n)
    (define aux
      (lambda (a n ret)
        (let ((ls (amicable_pair a)))
          (cond
            ((= a n) ret)
            ((null? ls) (aux (+ a 1) n ret))
            ((> (car ls) (cadr ls)) (aux (+ a 1) n ret))
            (else (aux (+ a 1) n (append ls ret)))))))
    (apply + (aux 1 n '()))))

;(p21 10000)

;; 22


(define name->score
  (lambda (str)
    (apply + (map (lambda (c) (- (char->integer c) 64)) (string->list str)))))

(define p22
  (lambda ()
    (let ((names (call-with-input-file "p022_names.txt"
                   (lambda (in) (read-line in)))))
     (let ((sorted_names (sort
                          (string-split
                           (string-replace
                            (string-replace
                             names
                             "\"" " " #:all? #t)
                            "," " " )) string<?)))
       (apply + (map * (map name->score sorted_names)
                       (range 1 (+ 1 (length sorted_names)))))))))
      
;; 23
;; 二つの過剰数の和で表せない正の整数の総和

;;divisors p21で定義済み
(define abundant_number?
  (lambda (n)
    (< n (apply + (cdr (divisors n))))))

(define abundant_numbers
  (lambda (n)
    (define aux
      (lambda (n m ret)
        (cond
          ((< n m) ret)
          ((abundant_number? m) (aux n (+ m 1) (append ret (list m))))
          (else (aux n (+ m 1) ret)))))
    (aux n 1 '())))

(define abundants (abundant_numbers 28123))

(define sum_of_two_abundant_number?
  (lambda (n)
    (define aux
      (lambda (n ls)
        (cond
          ((< n (* 2 (car ls))) #f)
          ((abundant_number? (- n (car ls))) #t)
          (else (aux n (cdr ls))))))
   (aux n abundants)))

(define p23
  (lambda ()
    (apply + (filter (lambda (x) (not (sum_of_two_abundant_number? x)))
                     (range 1 28123)))))

;(time (p23))  cpu time: 18867 real time: 18932 gc time: 206 遅い

;; 24

(define get-at
  (lambda (ls n)
    (if (zero? n) (car ls)
        (get-at (cdr ls) (- n 1)))))

(define remove-at
  (lambda (ls n)
    (define aux
      (lambda (ls n ret)
        (if (zero? n) (append ret (cdr ls))
            (aux (cdr ls) (- n 1) (append ret (list (car ls)))))))
    (aux ls n '())))

(define p24
  (lambda (n)
    (define aux
      (lambda (n m total ls ret)
        (let ((x (factorial (length (cdr ls)))))
          (cond
            ((null? (cdr ls)) (append ret ls))
            ((< n (+ x total))
             (aux n 1 total (remove-at ls (- m 1)) (append ret (list (get-at ls (- m 1))))))
            (else (aux n (+ m 1) (+ x total) ls ret))))))
    (apply string-append (map number->string (aux n 1 1 (range 10) '())))))

;(p24 1000000)

;; 25
;; 1000桁の一番小さいフィボナッチ数

; digit ... nは何桁か? 定義済み

(define p25
  (lambda (d)
    (define aux
      (lambda (n m d index)
        (let ((fi (+ n m)))
          (if (= d (digit fi)) index
              (aux m fi d (+ index 1))))))
    (aux 1 1 d 3)))

;(p25 3)
;(p25 1000)

;;26
;; d < 1000 において 1/d の循環小数の桁が最も大きいd

;循環小数 -> 余りを10倍にしたのを分母で割るを繰り返した時に
;同じ余りが出現したら循環小数となる。

(define member?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((eq? x (car ls)) #t)
      (else (member? x (cdr ls))))))

;(recurring_cycle 1 7) ->'(1 4 2 8 5 7)
(define recurring_cycle
  (lambda (y x)
    (define aux
      (lambda (y x mods quos)
        (let ((mod (modulo y x)))
          (cond
            ((member? mod mods)
             (drop (append quos (list (quotient y x)))
                   (+ 1 (index-of mods mod))))
            (else
             (aux (* 10 mod) x (append mods (list mod))
                  (append quos (list (quotient y x)))))))))
    (aux y x '() '())))

(define p26
  (lambda (n)
    (define aux
      (lambda (n d max ret)
        (let ((len (length (recurring_cycle 1 d))))
          (cond
            ((= n d) ret)
            ((< max len)
             (aux n (+ d 1) len d))
            (else
             (aux n (+ d 1) max ret))))))
    (aux n 2 0 0)))
;(p26 10)
;(p26 1000)

;; p27
;; n^2+an+b において　最も多くの素数を生成する aとbを求める
;; ただし |a| < 1000 かつ |b| <= 1000

(define p27
  (lambda ()
    (define aux
      (lambda (a b n max ret)
        (cond
          ((and (= a 999) (= b 1000)) ret)
          ((= b 1000) (aux (+ a 1) -1000 0 max ret))
          ((prime? (+ (* n n) (* n a) b)) (aux a b (+ n 1) max ret))
          (else
           (if (< max n) (aux a (+ b 1) 0 n (* a b))
               (aux a (+ b 1) 0 max ret))))))
    (aux -999 -1000 0 0 0)))

;(time (p27))

;; 28
;; 真ん中を1として渦巻状に整数を並べた1001×1001の正方形の対角線を結ぶの数字の和

(define p28
  (lambda (x)
    (define aux
      (lambda (x n m interval ret)
        (cond
          ((= n (* x x)) (+ ret n))
          ((= m 4) (aux x n 0 (+ interval 2) ret))
          (else (aux x (+ n interval) (+ m 1) interval (+ ret n))))))
    (aux x 3 1 2 1)))

;(p28 5)
;(p28 1001)

;; 29
;; a^b について 2 ≤ a ≤ 100 かつ 2 ≤ b ≤ 100 の時異なる値は幾つできるか?

(define list->set
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((member? (car ls) (cdr ls)) (list->set (cdr ls)))
      (else (cons (car ls) (list->set (cdr ls)))))))
;; (** a b) -> a^b 定義済み
(define p29
  (lambda (x)
    (define aux
      (lambda (x a b ls)
        (cond
          ((and (= a x) (= b x)) (cons (** a b) ls))
          ((= b x) (aux x (+ a 1) 2 (cons (** a b) ls)))
          (else (aux x a (+ b 1) (cons (** a b) ls))))))
    (length (list->set (aux x 2 2 '())))))

;(p29 5)
;(p29 100)

#| python

def p29(x):
    ret = set()
    for a in range(2, x+1):
        for b in range(2, x+1):
            ret.add(a ** b)
    return len(ret)

print(p29(5))
print(p29(100))

|#

;; 30
;; 各桁を5乗して足した数が元の数と一致する数字の合計

; 9^5 = 59049 6桁が上限
; 9999999 (7桁) のとき 9^5*7 = 413343 (6桁) にしかならない
; 調べる上限は　9^5 * 6 = 354294 とする。

;integer->list p16で定義　123->'(1 2 3)

(define p30
  (lambda ()
    (define aux
      (lambda (x ret)
        (cond
          ((= x 354294) ret)
          ((= x (apply + (map (lambda (n) (** n 5))
                              (integer->list x))))
           (aux (+ x 1) (+ ret x)))
          (else
           (aux (+ x 1) ret)))))
    (aux 2 0)))

;(p30)

;; 31
;; 1 2 5 10 20 50 100 のどれか(ひとつでも全部でも)使って200を表す通り

(define p31
  (lambda (x)
    (define aux
      (lambda (coins num sum x)
        (cond
          ((= 1 (car coins)) 1)
          ((= num 0)
           (aux (cdr coins) (quotient (- x sum) (cadr coins)) sum x))
          ((= (+ sum (* (car coins) num)) x)
           (+ 1 (aux coins (- num 1) sum x)))
          (else
           (+ (aux coins (- num 1) sum x)
              (aux (cdr coins)
                   (quotient (- x sum (* num (car coins))) (cadr coins))
                   (+ sum (* num (car coins)))
                   x))))))
    (aux '(200 100 50 20 10 5 2 1) (quotient x 200) 0 x)))

;(time (p31 200))

;; 32
;; かけられる数/かける数/積　の桁に1~9の数字を全て使って表せる積の合計
;; 例 39 × 186 = 7254
;;補足 15234 のような数字を 1~5のpandigital numberと言う

;; 50 と 2000は適当です。
;; どこまで調べればいいのか難しい。

(define p32
  (lambda ()
    (define aux
      (lambda (ret n m)
        (cond
          ((= n 50) ret)
          ((= m 2000) (aux ret (+ n 1) (+ n 1)))
          ((eqlist? (sort (append (integer->list n)
                                  (integer->list m)
                                  (integer->list (* n m)))
                          <)
                    (range 1 10))
           (aux (cons (* n m) ret) n (+ m 1)))
          (else (aux ret n (+ m 1))))))
    (apply + (list->set (aux '() 1 1)))))

;(time (p32))

;; 33
;; 49/98 -> 9をとる -> 4/8 -> 1/2
;; 49/98 -> 約分 -> 1/2
;; 上のような不思議な分数(4つある)の積の分母

(define rember
  (lambda (a ls)
    (cond
      ((null? ls) '())
      ((eq? a (car ls)) (cdr ls))
      (else
       (cons (car ls) (rember a (cdr ls)))))))

; 通分 (6 12) -> (1 2)
(define reduce-fraction
  (lambda (num den)
    (let ((x (gcd num den)))
      (list (/ num x) (/ den x)))))

(define curious_fraction?
  (lambda (num den)
    (let ((nl (integer->list num)))
    (let ((dl (integer->list den)))
    (let ((l (filter (lambda (x) (and (member? x nl) (member? x dl)))
                     (range 1 10))))
      (if (or (null? l) (zero? (car (rember (car l) dl))))
          #f
          (= (/ num den)
             (/ (car (rember (car l) nl)) (car (rember (car l) dl))))))))))

(define p33
  (lambda ()
    (define aux
      (lambda (ret num den)
        (cond
          ((= den 100) ret)
          ((= num den) (aux ret 11 (+ den 1)))
          ((curious_fraction? num den)
           (aux (cons (list num den) ret) (+ num 1) den))
          (else (aux ret (+ num 1) den)))))
    (second
     (apply reduce-fraction
           (apply map * (aux '() 11 11))))))

;(p33)

;; 34
;; 1! + 4! + 5! = 1 + 24 + 120 = 145 のような数字の合計
;; 1! = 1 2! = 2 は含めない
;; 999999(6桁) -> (* (factorial 9) 6) 2177280(7桁)
;; 9999999(7桁) -> (* (factorial 9) 7) 2540160(7桁)
;; 99999999(8桁) -> (* (factorial 9) 8) 2903040(7桁)
;; 8桁以降はありえない

(define p34
  (lambda ()
    (define aux
      (lambda (ret n)
        (cond
          ((= n 2540160) ret)
          ((= n (apply + (map factorial (integer->list n))))
           (aux (cons n ret) (+ n 1)))
          (else
           (aux ret (+ n 1))))))
    (apply + (aux '() 10))))

;(time (p34))  ;cpu time: 3027 real time: 3043 gc time: 73

;; 35
;; 1000000 以下のcircular primeの個数
;; circular prime 例197  -> 197, 971, 719 全部素数

;'(1 2 3) -> 123
(define list->integer
  (lambda (ls)
    (string->number (apply string-append (map number->string ls)))))

; 全てパターンじゃなくてローテーションだった
(define asp-aux
  (lambda (ls l)
    (if (null? ls)
        '()
        (append (map (lambda (xs) (cons (car ls) xs))
                     (all-sort-patterns (rember (car ls) l)))
                (asp-aux (cdr ls) l)))))

(define all-sort-patterns
  (lambda (ls)
    (if (null? (cdr ls))
        (list ls)
        (asp-aux ls ls))))

(define rotations
  (lambda (ls)
    (define aux
      (lambda (ret ls)
        (if (eqlist? (car ret) ls)
            ret
            (aux (append ret (list ls))
                 (append (cdr ls) (list (car ls)))))))
    (aux (list ls) (append (cdr ls) (list (car ls))))))

(define circular_prime?
  (lambda (n)
    (define aux
      (lambda (ls)
        (cond
          ((null? ls) #t)
          ((not (prime? (car ls))) #f)
          (else (aux (cdr ls))))))
    (aux (map list->integer (rotations (integer->list n))))))

;上の方が微妙に早い
;(define circular_prime?
;  (lambda (n)
;    (null? (filter (lambda (bool) (eq? #f bool))
;                   (map prime?
;                        (map list->integer
;                             (rotations (integer->list n))))))))

(define p35
  (lambda (x)
    (count circular_prime? (primes_under x))))

;(time (p35 1000000))
;cpu time: 3115 real time: 3125 gc time: 67 遅い〜


;; 36
;; 1000000未満の10進数と2進数両方とも回文となる数字の合計
(define integer->binary
  (lambda (x)
    (define aux
      (lambda (ret x)
        (if (zero? x)
            ret
            (aux (string-append (number->string (modulo x 2)) ret)
                 (quotient x 2)))))
    (string->number (aux "" x))))

;palindromic_number? 定義済み
(define palindromic_base_10and2?
  (lambda (x)
    (and (palindromic_number? x)
         (palindromic_number? (integer->binary x)))))

(define p36
  (lambda (x)
    (apply + (filter palindromic_base_10and2? (range 1 (+ x 1))))))

;(time (p36 1000000)) ;cpu time: 296 real time: 298 gc time: 60

;; 37
;; 3797 のように 3797(素数) 797(素数) 97(素数) 7(素数)
;; 379(素数) 37(素数) 3(素数)となるような 11個の素数の合計

(define prime_at_each_stage?
  (lambda (x)
    (define aux
      (lambda (ls drop_LorR)
        (cond
          ((null? ls) #t)
          ((prime? (list->integer ls))
           (aux (drop_LorR ls 1) drop_LorR))
          (else #f))))
    (let ((ls (integer->list x)))
      (and (aux ls drop-right)
           (aux ls drop)))))

(define p37
  (lambda ()
    (define aux
      (lambda (ret n count)
        (cond
          ((= count 11) ret)
          ((prime_at_each_stage? n)
           (aux (+ ret n) (+ n 2) (+ count 1)))
          (else
           (aux ret (+ n 2) count)))))
    (aux 0 11 0)))

;(time (p37)) ;cpu time: 1400 real time: 1411 gc time: 113

;; 38
;; 192*1 = 192 / 192*2 =384 / 192*3 = 576 /(連結)-> 192384576 (パンデジタル)
;; このように x と (1,2,...,n) によって作られる 9桁の パンデジタル数の最大

(define pandigital?
  (lambda (x n)
    (eqlist?
     (sort (integer->list x) <)
     (range 1 (+ n 1)))))

(define p38
  (lambda ()
    (define aux
      (lambda (ret x n)
        (let ((m (list->integer (map (lambda (a) (* x a)) (range 1 (+ n 1))))))
          (cond
            ((and (pandigital? m 9) (< ret m))
                (aux m x (+ n 1)))
            ((= n 2)
             (if (> m 990000000)
                 ret
                 (aux ret x (+ n 1))))
            (else
             (if (or (= m 10) (> m 990000000))
                 (aux ret (+ x 1) 2)
                 (aux ret x (+ n 1))))))))
    (aux 0 1 2)))

;(time (p38)) ;cpu time: 53 real time: 53 gc time: 3

;; 39
;; 辺の長さ(a b c)の直角三角形について (p =) a+b+c = 120の時
;; (a b c)の組み合わせは (20 48 52),(24 45 51),(30 40 50)の三つある
;; p<=1000 の時(a b c)の組み合わせが最も多くなるpの値

;a <= b < c とする
(define right_triangle?
  (lambda (a b c)
    (= (* c c) (+ (* a a) (* b b)))))

; pにおける直角三角形の数
(define num_of_right_triangle
  (lambda (p)
    (define aux
      (lambda (ret p a b)
        (cond
          ((and (= a b) (> b (- p a b))) ret)
          ((right_triangle? a b (- p a b))
           (aux (+ ret 1) p (+ a 1) (+ a 1)))
          ((> b (- p a b))
           (aux ret p (+ a 1) (+ a 1)))
          (else
           (aux ret p a (+ b 1))))))
    (aux 0 p 1 1)))

(define p39
  (lambda ()
    (define aux
      (lambda (ret max p)
        (let ((n (num_of_right_triangle p)))
          (cond
            ((< 1000 p) ret)
            ((< max n)
             (aux p n (+ p 1)))
            (else
             (aux ret max (+ p 1)))))))
    (aux 0 0 3)))

;(time (p39)) ;cpu time: 1736 real time: 1731 gc time: 4
;; もっと早くできるかも

;; 40
;; 0.123456789101112131415161718192021...
;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

(define nth_digit_over
  (lambda (n)
    (define aux
      (lambda (x sum n)
        (if (>= sum n)
            (list x sum)
            (aux (+ x 1) (+ sum (digit (+ 1 x))) n))))
    (aux 1 1 n)))

(define num_of_nth_digit
  (lambda (n)
    (let ((ls (nth_digit_over n)))
      (last (drop-right (integer->list (first ls))
                        (- (second ls) n))))))


(define p40
  (lambda ()
    (apply * (map num_of_nth_digit
                  '(1 10 100 1000 10000 100000 1000000)))))

;(time (p40))  ;cpu time: 55 real time: 56 gc time: 19

;; 41
;; パンデジタルの中で最大の素数は?

;all-sort-patterns は35で定義済み
(define p41
  (lambda ()
    (define aux
      (lambda (n)
        (let ((ls (filter prime?
                          (map list->integer
                               (permutations (range 1 n))))))
          (if (null? ls)
              (aux (- n 1))
              (apply max ls)))))
    (aux 10)))

;(time (p41))  ;cpu time: 1688 real time: 1692 gc time: 881

;; 42
;; ファイルの中に三角語がいくつあるか

;A->1 B->2  "ABC"->6 
(define word_value
  (lambda (str)
    (apply + (map (lambda (c) (- (char->integer c) 64))
                  (string->list str)))))

(define triangle_number
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

(define triangle_numbers_under
  (lambda (x)
    (define aux
      (lambda (ret x n)
        (let ((tn (triangle_number n)))
          (if (< x tn)
              ret
              (aux (cons tn ret) x (+ n 1))))))
    (aux '() x 1)))

(define p42
  (lambda ()
    (let ((words-file (call-with-input-file "p042_words.txt"
                   (lambda (in) (read-line in)))))
     (let ((words (string-split
                   (string-replace
                    (string-replace
                     words-file
                      "\"" " " #:all? #t)
                       "," " " ))))
       (let ((values
              (map word_value words)))
         (let ((t_numbers
                (triangle_numbers_under (apply max values))))
           (count (lambda (value) (member? value t_numbers))
                  values)))))))

;(time (p42))  ;cpu time: 5 real time: 5 gc time: 0

;; 43
;; めちゃくちゃ遅い。やり直す。

(define p43-aux
  (lambda (ls)
     (define aux
       (lambda (ls l)
         (cond
           ((null? l) #t) 
           ((not (zero? (modulo (list->integer (take ls 3))
                                (car l)))) #f)
           (else (aux (cdr ls) (cdr l))))))
     (aux (cdr ls) '(2 3 5 7 11 13 17))))

(define p43
  (lambda ()
    (let ((lls (all-sort-patterns (range 0 10))))
      (apply +
             (map list->integer
                  (filter p43-aux lls))))))

;(time (p43)) ;cpu time: 16640 real time: 13920 gc time: 9602

;; 44
;; 五角数 Pn = n(3n-1)/2
;; PjとPkについて 和と差は五角数になるペアのうち差が最小となる時の差
(define pentagonal?
  (lambda (n)
    (let ((a (sqrt (+ (* 24 n) 1))))
      (if (and (integer? a)
               (zero? (modulo (+ a 1) 6)))
          #t
          #f))))

(define pentagonal
  (lambda (n)
    (/ (* n (- (* n 3) 1)) 2)))

(define p44
  (lambda ()
    (define aux
      (lambda (j k)
        (cond
          ((= k 0) (aux (+ j 1) j))
          ((and (pentagonal? (- (pentagonal j) (pentagonal k)))
                (pentagonal? (+ (pentagonal k) (pentagonal j))))
           (- (pentagonal j) (pentagonal k)))
          (else (aux j (- k 1))))))
    (aux 1 1)))

(time (p44))  ;cpu time: 469 real time: 469 gc time: 8



;; 45
;; 三角数Tn=n(n+1)/2, 五角数Pn=n(3n-1)/2, 六角数Hn=n(2n-1)
;; T285 = P165 = H143 = 40755
;; この次の三角数かつ五角数かつ六角数を求める
;; 六角数は三角数でもあるので実質、五角数かつ六角数を求める


; 遅くて使えなかった
; sqrt があったからいらなかった
; 整数になるもの限定 整数にならなければ #fを返す
; (root 3) -> #f  (root 25) -> 5
(define root
  (lambda (x)
    (define aux
      (lambda (x n)
        (cond
          ((> (* n n) x) #f)
          ((= (* n n) x) n)
          (else
           (aux x (+ n 1))))))
    (aux x 1)))

(define hexagonal?
  (lambda (n)
    (let ((a (sqrt (+ (* 8 n) 1))))
      (if (and (integer? a)
               (zero? (modulo (+ a 1) 4)))
          #t
          #f))))

(define p45
  (lambda ()
    (define aux
      (lambda (n)
        (let ((pen (pentagonal n)))
        (if (hexagonal? pen)
             pen
            (aux (+ n 1))))))
    (aux 166)))

(time (p45))  ;cpu time: 5 real time: 5 gc time: 0

;; 46
;; 合成数は1を除く素数以外の数字
;; 平方数の2倍と素数の和で表せない最小の奇合成数は？

(define p46
  (lambda ()
    (define aux
      (lambda (x n)
        (cond
          ((and (= n 1) (prime? x))
           (aux (+ x 2) 1))
          ((< x (* 2 n n)) x)
          ((prime? (- x (* 2 n n)))
           (aux (+ x 2) 1))
          (else
           (aux x (+ n 1))))))
    (aux 3 1)))

(time (p46))  ;cpu time: 5 real time: 5 gc time: 0

;; 47
;; それぞれ4つの異なる素因数を持つ連続する4つの数の最初の数は？

;(prime_factor 644) -> '(23 7 2 2)
;(prime_factor* 644) -> '(4 7 23)

(define prime_factor*
  (lambda (x)
    (define aux
      (lambda (ret ls n)
        (cond
          ((null? ls) ret)
          ((= (car ls) n)
           (aux (cons (* (car ls) (car ret)) (cdr ret))
                (cdr ls) n))
          (else
           (aux (cons (car ls) ret) (cdr ls) (car ls))))))
    (aux '() (prime_factor x) 0)))

#|
;いらなかった問題を16個の数字全て違わないといけないと思ってた。
;同じ要素を含むかどうか
(define contain_same?
  (lambda (l1 l2)
    (cond
      ((null? l1) #f)
      ((member? (car l1) l2) #t)
      (else
       (contain_same? (cdr l1) l2)))))

;(p47-aux '((1 2 3)(4 5 6)(7 8 9)) '(8 a b)) -> 3
(define p47-aux
  (lambda (lls l)
    (cond
      ((null? lls) 0)
      ((contain_same? (last lls) l) (length lls))
      (else (p47-aux (drop-right lls 1) l)))))

(define p47
  (lambda (a)
    (define aux
      (lambda (a n ls)
        (let ((pf (prime_factor* n)))
          (cond
            ((= a (length ls)) (- n a))
            ((not (= a (length pf)))
             (aux a (+ n 1) '()))
            (else
             (let ((x (p47-aux ls pf)))
               (if (= x 0)
                   (aux a (+ n 1)
                        (append ls (list pf)))
                   (aux a (+ n 1)
                        (drop (append ls (list pf)) x)))))))))
    (aux a 2 '())))
|#
(define p47
  (lambda (x)
    (define aux
      (lambda (x n c)
        (let ((pf (prime_factor* n)))
          (cond
            ((= x c) (- n x))
            ((= x (length pf))
             (aux x (+ n 1) (+ c 1)))
            (else
             (aux x (+ n 1) 0))))))
    (aux x 2 0)))

;(time (p47 3))
;(time (p47 4)) ;cpu time: 5098 real time: 5115 gc time: 5

;; 48
;; 1^1 + 2^2 + 3^3 + ... + 1000^1000 の下位10桁

;1^1 + 2^2 + 3^3 + ... + n^n 
(define series
  (lambda (n)
    (apply + (map (lambda (x) (** x x)) (range 1 (+ n 1))))))

(define p48
  (lambda (n)
    (list->integer
     (take-right (integer->list (series n)) 10))))

(time (p48 1000))  ;cpu time: 94 real time: 95 gc time: 8

;; 49
;; 差3330の等差数列 1487,4817,8147の三つの項は 全て素数で、各項は他の項の各桁の並べ替えで表される。
;; このような4桁の数を見つけて、12桁で表す。

(define permutation_numbers?
  (lambda (ls)
    (define aux
      (lambda (lls)
        (cond
          ((null? (cdr lls)) #t)
          ((eqlist? (car lls) (cadr lls))
           (aux (cdr lls)))
          (else #f))))
    (aux (map (lambda (num) (sort (integer->list num) <)) ls))))

(define p49
  (lambda ()
    (define aux
      (lambda (d n)
        (cond
          ((< 9999 (+ n d d)) (aux (+ d 1) 1000))
          ((and (prime? n) (prime? (+ n d)) (prime? (+ n d d))
                (permutation_numbers? (list n (+ n d) (+ n d d))))
           (if (= n 1487) (aux d (+ n 1))
               (list->integer (list n (+ n d) (+ n d d)))))
          (else (aux d (+ n 1))))))
    (aux 1 1000)))

;(time (p49)) ;cpu time: 6261 real time: 6283 gc time: 8
;項差は3330で固定でいいのか。それなら一瞬だけど

;; 50
;; 41(素数)= 2+3+5+7+11+13 6この連続する素数の和で表せる 100未満の時最長
;; 1000未満の時 953が21の連続の素数で表せられる.
;; 1000000未満では?

; nからそれ以下の素数について、合計が素数となる最長のchainと合計を求める
; (max_chain_p50 13 100) -> '(6 41)
(define max_chain_p50
  (lambda (n x)
    (define aux
      (lambda (ret sum c n x)
        (cond
          ((< x sum) ret)
          ((= n 1)
           (if (prime? sum)
               (list c sum)
               ret))
          ((not (prime? n)) (aux ret sum c (- n 1) x))
          (else
           (if (prime? sum)
               (aux (list c sum)
                    (+ sum n) (+ c 1) (- n 1) x)
               (aux ret (+ sum n) (+ c 1) (- n 1) x))))))
    (aux '(0 0) n 1 (- n 1) x)))

(define p50
  (lambda (x)
    (cadar (sort
            (map (lambda (pn) (max_chain_p50 pn x)) (primes_under x))
            (lambda (ls l) (> (car ls) (car l)))))))

;(time (p50 1000000))
;cpu time: 7633 real time: 7660 gc time: 84
;もっと早くできそう

;; 51
;; 56oo3 のoの部分を 0 ~ 9で置換すると
;; 56003, 56113, 56333, 56443, 56663, 56773, 56993 の7つの素数ができる
;; このように8つの素数があるパターンを見つける

;(insert '(5 6 3) '(2 3) 0)-> (5 6 0 0 3)
(define insert
  (lambda (ls l x)
    (define aux
      (lambda (ret ls l x n)
        (cond
          ((null? l) (append ret ls))
          ((= (car l) n) (aux (append ret (list x)) ls (cdr l) x (+ n 1)))
          (else (aux (append ret (list (car ls))) (cdr ls) l x (+ n 1))))))
    (aux '() ls l x 0)))

;(count_prime_p51 '(5 6 3) '(2 3)) -> 7
(define count_prime_p51
  (lambda (ls l)
    (cond
      ((zero? (car ls)) 0)
      ((zero? (car l)) (count prime?
                              (map list->integer
                                   (map (lambda (x) (insert ls l x))
                                        (range 1 10)))))
      (else (count prime? (map list->integer
                               (map (lambda (x) (insert ls l x))
                                    (range 10))))))))

(define p51-aux
  (lambda (ls l num)
    (define aux
      (lambda (ls l lkeep num)
        (cond
          ((null? ls) '())
          ((null? l) (aux (cdr ls) lkeep lkeep num))
          ((= num (count_prime_p51 (car ls) (car l)))
           (apply min (filter prime?
                              (map list->integer
                                   (map (lambda (x) (insert (car ls) (car l) x))
                                        (range 10))))))
          (else (aux ls (cdr l) lkeep num)))))
    (aux ls l l num)))


(define p51
  (lambda (num)
    (define aux
      (lambda (digit no num)
          (let ((ls (combinations (flatten (for/list ([i (range (- digit no))])
                                             (range 10))) (- digit no))))
            (let ((l (combinations (range (- digit 1)) no)))
              (println (list digit no))
              (let ((result (p51-aux ls l num)))
                (cond
                  ((not (null? result)) result)
                  ((= no (- digit 1)) (aux (+ digit 1) 1 num))
                  (else (aux digit (+ no 1) num))))))))
    (aux 2 1 num)))

;(time (p51 8))


;; 52
;; 2x, 3x, 4x, 5x, 6x が x と同じ数を含む最小の整数 x

; lls ->'((1 2 3) (1 2 3) (1 2 4))
(define eqlist?-all
  (lambda (lls)
    (if (null? (filter-not
                (lambda (l) (eqlist? (car lls) l))
                (cdr lls)))
        #t
        #f)))

(define p52
  (lambda (n)
    (define aux
      (lambda (n x)
        (if (eqlist?-all (map (lambda (num) (sort (integer->list num) <))
                              (map * (make-list n x) (range 1 (+ n 1)))))
            x
            (aux n (+ x 1)))))
    (aux n 1)))

(time (p52 6))  ;cpu time: 839 real time: 843 gc time: 30

;; 53
;; 1 <= n <= 100 の間に nCrが1000000を超えるのは何通り
; (combination n r) 定義済み

(define p53
  (lambda (x)
    (apply + (map (lambda (cs) (count (lambda (c) (<= x c))
                                      cs))
                  (map (lambda (rs)
                         (map (lambda (r) (combination (car rs) r))
                              rs)) 
                        (map (lambda (n) (range n 0 -1))
                             (range 1 101)))))))

(time (p53 1000000))  ;cpu time: 32 real time: 33 gc time: 5

;; 54
;; ポーカーの勝敗

; '(1 1 1 1) -> #t
(define eq?-all
  (lambda (ls)
    (if (null? (filter-not
                (lambda (l) (eq? (car ls) l))
                (cdr ls)))
        #t
        #f)))

(define rember*
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eq? x (car ls)) (rember* x (cdr ls)))
      (else (cons (car ls) (rember* x (cdr ls)))))))

(define read-lines
  (lambda (file-name)
    (call-with-input-file file-name
      (lambda (p)
        (let loop ((ls '()) (line (read-line p)))
          (if (eof-object? line)
              (begin
                (close-input-port p)
                ls)
              (loop (append ls (list line)) (read-line p))))))))

(define hash54
  (hash "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
        "T" 10 "J" 11 "Q" 12 "K" 13 "A" 14))

; (card-encode "8D")->(8 "D")
; (card-encode "KS") -> (13 "S")
(define card-encode
  (lambda (card)
    (let ((l (map string (string->list card))))
      (cons (hash-ref hash54 (car l))
            (cdr l)))))

;'(11 4 11 6 4) -> '((2 11) (2 4) (1 6))
(define nums-encode
  (lambda (nums)
    (define aux
      (lambda (ret nums)
        (if (null? nums)
            ret
            (aux (cons (list (count (lambda (x) (eq? (car nums) x)) nums)
                             (car nums))
                       ret)
                 (rember* (car nums) nums)))))
    (sort (aux '() nums)
          (lambda (l1 l2) (> (car l1) (car l2))))))

; 連続した数字か? '(1 4 3 2 5) -> #t
(define consecutive?
  (lambda (ls)
    (let ((sorted (sort ls <)))
      (eqlist? sorted
               (range (car sorted) (+ (length sorted) (car sorted)))))))


(define numof
  (lambda (ls) (car ls)))

(define valof
  (lambda (ls) (cadr ls)))

(define porker_eval
  (lambda (5card)
    (let ((cards (map card-encode 5card)))
      (let ((marks (map cadr cards)))
        (let ((nums (map car cards)))
          (let ((encnums (nums-encode (sort nums >))))
            (cond
              ((consecutive? nums)
               (if (eq?-all marks)
                   (+ (apply max nums) 800)  ;Straight Flush
                   (+ (apply max nums) 400)))    ;Straight
              ((= (numof (first encnums)) 4)       
               (+ (valof (first encnums)) 700))   ;Four of a Kind
              ((= (numof (first encnums)) 3)       
               (if (= (numof (second encnums)) 2)   
                   (+ (valof (first encnums)) 600)  ;Full House
                   (+ (valof (first encnums)) 300)))  ;Three of a Kind
              ((eq?-all marks)
               (+ (valof (first encnums)) 500))  ;Flush
              ((= (numof (first encnums)) 2)
               (if (= (numof (second encnums)) 2)
                   (+ (valof (first encnums)) 200)  ;Two Pairs
                   (+ (valof (first encnums)) 100)))  ;One Pair
              (else (sort nums >))))))))) ;ペアがないときは numsをそのまま返す

; p1 と p2どっちが勝つか
(define poker_winer
  (lambda (10cards)
    (let ((p1 (porker_eval (take 10cards 5))))
      (let ((p2 (porker_eval (take-right 10cards 5))))
        (cond
          ((and (list? p1) (list? p2))
           (if (< 0 (car (filter-not zero? (map - p1 p2))))
               1
               2))
          ((list? p1) 2)
          ((list? p2) 1)
          ((= p1 p2) 0)
          (else (if (< p1 p2) 2 1)))))))

(define p54
  (lambda ()
    (let ((lines (map string-split
                      (read-lines "p054_poker.txt"))))
      (count (lambda (n) (= n 1)) (map poker_winer lines)))))

(time (p54)) ;cpu time: 14 real time: 14 gc time: 0

;; 55
;; Lychrel数 ある数とその数を反転させた数を足すことを何度(50未満)繰り返しても答えが回文数にならない数
;; 10000未満のLychrel数の個数は?

;palindromic_number? (回文数か) はp4で定義

(define lychrel?
  (lambda (x)
    (define aux
      (lambda (x n)
        (let ((a (+ x (list->integer
                       (reverse (integer->list x))))))
          (cond
            ((= n 50) #t)
            ((palindromic_number? a) #f)
            (else (aux a (+ n 1)))))))
    (aux x 1)))

(define p55
  (lambda (n)
    (count lychrel? (range 1 n))))

(time (p55 10000)) ;cpu time: 74 real time: 74 gc time: 4

;; 56
;; a, b < 100 において a^b の数字和(桁の総和)の最大値は?

; sum_digits はp16で定義 (sum_digits 36288) -> 27

(define p56
  (lambda (a b)
    (apply max
           (map sum_digits
                    (flatten
                     (map (lambda (x)
                            (map (lambda (y) (** x y))
                                 (range 1 b)))
                          (range 1 a)))))))

(time (p56 100 100)) ;cpu time: 85 real time: 85 gc time: 5

;; 57
;; ルート2の問題

(define root2
  (lambda (n)
    (define aux
      (lambda (n)
        (if (= 1 n)
            2
            (+ 2 (/ 1 (aux (- n 1)))))))
    (+ 1 (/ 1 (aux n)))))

(define p57
  (lambda (n)
    (count (lambda (x)
             (let ((a (root2 x)))
               (> (digit (numerator a)) (digit (denominator a)))))
           (range 1 (+ n 1)))))

(time (p57 1000)) ;cpu time: 109 real time: 109 gc time: 7

;; 58
;; 渦巻に並べた数字で対角線上の素数の割合が10%以下になる時の辺の長さ

(define p58
  (lambda ()
    (define aux
      (lambda (side x c pn nn)
        (cond
          ((< (/ pn (+ pn nn)) 0.1) side)
          ((= c 4)
           (if (prime? x)
               (aux (+ side 2) (+ x side 1) 1 (add1 pn) nn)
               (aux (+ side 2) (+ x side 1) 1 pn (add1 nn))))
          
          (else
           (if (prime? x)
               (aux side (+ x (- side 1)) (add1 c) (add1 pn) nn)
               (aux side (+ x (- side 1)) (add1 c) pn (add1 nn)))))))
    (aux 5 13 1 3 2)))

;(time (p58))  ;cpu time: 2302 real time: 2309 gc time: 0


;; 59
;; 問題がよくわからない
;; 後回し

(define read-csv
  (lambda (file)
    (let* ((csv-file (call-with-input-file file
                   (lambda (in) (read-line in))))
           (str-list (string-split
                      (string-replace
                       (string-replace
                        csv-file
                        "\"" " " #:all? #t)
                        "," " " ))))
      str-list)))

;(read-csv "p059_cipher.txt")


;; 60
;; 3, 7, 109, 673 は任意の二つのの素数を任意の順番でくっつけた値が素数になる4つの素数の集合
;; このような5つの素数の集合の和で最小となるのは

#|
; lsが p60の性質を持つ素数の集合か判定
; (set60? '(3 7 109 673)) -> #t
(define set60?
  (lambda (ls)
    (define aux
      (lambda (l)
        (cond
          ((null? l) #t)
          ((not (and (prime? (list->integer (car l)))
                     (prime? (list->integer (reverse (car l))))))
           #f)
          (else (aux (cdr l))))))
    (aux (combinations ls 2))))

(define p60
  (lambda (n)
    (define aux
      (lambda (ls x n)
        (cond
          ((not (prime? x)) (aux ls (+ x 2) n))
          (else
           (let ((ret (filter set60?
                              (map (lambda (l) (cons x l))
                                   (combinations ls (- n 1))))))
             (if (null? ret)
                 (aux (cons x ls) (+ x 2) n)
                 (begin (println ret)
                        (apply min (map (lambda (l) (apply + l)) ret)))))))))
    (aux '(2) 3 n)))

;(time (p60 4))
;(time (p60 5))
; これでも多分答え出るけど、遅すぎる
; いったんギブアップ
|#

; (prime?-all-union '(3 7) 109) -> #t
;やってること (and (prime? 3109)　(prime? 1093) (prime? 7109) (prime? 1097))
(define prime?-all-union
  (lambda (ls n)
    (cond
      ((null? ls) #t)
      ((not (and (prime? (list->integer (list (car ls) n)))
                 (prime? (list->integer (list n (car ls))))))
       #f)
      (else (prime?-all-union (cdr ls) n)))))

;(() (条件を満たす (n-1)個セットのリスト) .. (2個セットのリスト) ((list 素数)のリスト))
;を作りながら繰り返す。一番左の () に(条件を満たすn個のセット)が入ったらそれを返す。
;例　'(() () ((7 3) (11 3) (17 3)) ((3) (5) (7) (11) (13) (17)))
(define p60-aux
  (lambda (x lls)
    (cond
      ((not (prime? x)) (p60-aux (+ x 2) lls))
      ((not (null? (car lls))) (car lls))
      (else
        (let* ((ls (map (lambda (l) (filter (lambda (li) (prime?-all-union li x))
                                            l))
                          lls))
               (xls (append
                     (map (lambda (l) (map (lambda (li) (cons x li))
                                           l))
                          (cdr ls))
                     (list (list (list x)))))
               (lls2 (map append lls xls)))
          (p60-aux (+ x 2) lls2))))))

(define p60
  (lambda (n)
    (apply + (car (p60-aux 3 (make-list n '()))))))

;(time (p60 5))  ;cpu time: 246939 real time: 165626 gc time: 5724
;; 61
(define triangle
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

(define square
  (lambda (n)
    (* n n)))

(define hexagonal
  (lambda (n)
    (* n (- (* 2 n) 1))))

(define heptagonal
  (lambda (n)
    (/ (* n (- (* 5 n) 3)) 2)))

(define octagonal
  (lambda (n)
    (* n (- (* 3 n) 2))))


; 1234 -> ("12" "34")
(define split-4digit->2digit
  (lambda (n)
    (let ((s (number->string n)))
      (list (substring s 0 2) (substring s 2 4)))))


;'(("12" "34") ("12" "99") ("56" "78"))
;-> #hash(("12" . ("34" "99")) ("56" . ("78")))
(define set-hash-61
  (lambda (h ls)
     (if (null? ls)
         h
         (begin (hash-set! h (caar ls)
                           (cons (cadar ls)
                                 (hash-ref! h (caar ls) '())))
                (set-hash-61 h (cdr ls))))))

; triangle squareなどの関数をを引数にとり
; その関数における4桁の数のリストを作る
; (Xtagonal_4digits triangle) -> '(1035 1081 1128 ...  9591 9730 9870)
(define Xtagonal_4digits
  (lambda (func)
    (define aux
      (lambda (ret func n)
        (cond
          ((<= 10000 (func n)) ret)
          ((<= 1000 (func n))
           (aux (append ret (list (func n))) func (+ n 1)))
          (else (aux ret func (+ n 1))))))
    (aux '() func 1)))


(define p61-aux2
  (lambda (ret hashs)
    (cond
      ((null? hashs)
       (if (eq? (string->number (substring (last ret) 2 4))
                (string->number (substring (car ret) 0 2)))
           ret
           '()))
      ((not (hash-has-key? (car hashs) (substring (last ret) 2 4))) '())
      (else (filter-not
             null?
             (map (lambda (aa)
                    (p61-aux2 (append ret (list (string-append (substring (last ret) 2 4) aa)))
                              (cdr hashs)))
                  (hash-ref (car hashs) (substring (last ret) 2 4))))))))

(define p61-aux
  (lambda (ls hs l)
    (if (null? ls)
        (p61-aux l (cdr hs) l)
        (let ((ans (p61-aux2 (list (string-append (caar ls) (cadar ls))) (car hs))))
          (if (not (null? ans))
              ans
              (p61-aux (cdr ls) hs l))))))

(define p61
  (lambda ()
    (let* ((oct (Xtagonal_4digits octagonal))
           (hep (Xtagonal_4digits heptagonal))
           (hex (Xtagonal_4digits hexagonal))
           (pen (Xtagonal_4digits pentagonal))
           (squ (Xtagonal_4digits square))
           (tri (Xtagonal_4digits triangle))
           
           (hep2 (map split-4digit->2digit hep))
           (hex2 (map split-4digit->2digit hex))
           (pen2 (map split-4digit->2digit pen))
           (squ2 (map split-4digit->2digit squ))
           (tri2 (map split-4digit->2digit tri))
           
           (oct_ls (map split-4digit->2digit oct))
           (hep_h (set-hash-61 (make-hash) hep2))
           (hex_h (set-hash-61 (make-hash) hex2))
           (pen_h (set-hash-61 (make-hash) pen2))
           (squ_h (set-hash-61 (make-hash) squ2))
           (tri_h (set-hash-61 (make-hash) tri2)))
      (let ((ret (p61-aux  oct_ls
                           (permutations (list hep_h hex_h pen_h squ_h tri_h))
                           oct_ls)))
        (println (flatten ret))
        (apply + (map string->number (flatten ret)))))))

(time (p61))
;cpu time: 4 real time: 5 gc time: 0


;; 62
;; 41063625は 345^3 で　桁を入れ替えた　56623104は 384^3 さらに　66430125は 405^3
;; このように立方数になる桁の置換が5つある最小の立方数は?

(define p62
  (lambda (c)
    (define h (make-hash))
    (define aux
      (lambda (c n)
        (let ((x (list->integer
                  (sort (integer->list (* n n n)) >))))
          (cond
            ((not (hash-has-key? h x))
             (begin (hash-set! h x (list n))
                    (aux c (+ n 1))))
            ((begin (hash-set! h x (cons n (hash-ref h x)))
                    #f))
            ((= c (length (hash-ref h x)))
             (apply min (map (lambda (x) (** x 3))
                             (hash-ref h x))))
            (else (aux c (+ n 1)))))))
    (aux c 1)))

;(time (p62 5))  ;cpu time: 30 real time: 30 gc time: 5

;; 63
;; 自然数xをn乗して得られるn桁の整数は何個あるか?
;; 10は2乗で3桁, 3乗で4桁となるのでxは9まで

(define p63
  (lambda ()
    (define aux
      (lambda (ret x n)
        (let ((d (digit (** x n))))
          (cond
            ((= x 10) ret)
            ((> n d) (aux ret (+ x 1) 1))
            ((= n d) (aux (+ ret 1) x (+ n 1)))
            (else (aux ret x (+ n 1)))))))
    (aux 0 1 1)))
;(time (p63)) ;cpu time: 0 real time: 0 gc time: 0

;; 64

;; 65

(define convergent-e
  (lambda (ls)
    (define (aux ls)
      (if (null? (cdr ls))
          (car ls)
          (+ (car ls) (/ 1 (aux (cdr ls))))))
    (if (null? ls)
        2
        (+ 2 (/ 1 (aux ls))))))

(define e-nth
  (lambda (nth)
    (let* ((ls (flatten (map (lambda (k) (list 1 (* 2 k) 1))
                            (range 1 (+ 2 (quotient nth 3))))))
           (len (length ls))
           (l (drop-right ls (+ 1 (- len nth)))))
      (convergent-e l))))

(define p65
  (lambda (nth)
    (apply + (integer->list (numerator (e-nth nth))))))

;(time (p65 10))
(time (p65 100))
;cpu time: 0 real time: 0 gc time: 0

;; 66

;; 67
;; 18と同じ最大となる経路を求める
;; でも18と同じじゃ計算が終わらない

;read-lines (p54)

;(list-split '(1 2 3 4 5 6) 2) -> '((1 2) (3 4) (5 6))
(define list-split
  (lambda (ls n)
    (define (aux ret ls n)
       (if (null? ls)
           ret
           (aux (append ret (list (take ls n))) (drop ls n) n)))
    (aux '() ls n)))

(define p67
  (lambda ()
    (define (aux tri row)
      (if (null? tri)
          (car row)
          (aux (cdr tri)
               (map + (car tri)
                      (map (lambda (l) (apply max l))
                           (list-split (drop-right
                                        (drop
                                         (flatten (map (lambda (x) (list x x))row))
                                              1) 1)
                                       2))))))
    
    (let ((inverted_triangle
           (map (lambda (ls) (map (lambda (s) (string->number s)) ls))
                (map string-split
                     (read-lines "p067_triangle.txt")))))
      (aux (cdr inverted_triangle) (car inverted_triangle)))))

;(time (p67))  ;cpu time: 4 real time: 4 gc time: 0

;; 68

;; 69

;定義通り(nと互いに素なi以上n以下の自然数の個数)
;(define totient
;  (lambda (n)
;    (count (lambda (x) (= 1 (gcd n x))) (range 1 n))))

;公式
(define totient
  (lambda (n)
    (if (= n 1)
        1
        (let ((pf (list->set (prime_factor n))))
          (apply * (cons n (map (lambda (p) (- 1 (/ 1 p))) pf)))))))

(define p69
  (lambda (n)
    (define (aux ret max n)
      (cond
        ((= n 1) ret)
        ((< max (/ n (totient n)))
         (aux n (/ n (totient n)) (- n 1)))
        (else
         (aux ret max (- n 1)))))
    (aux 0 0 n)))

;(time (p69 1000000))  ;cpu time: 204247 real time: 204932 gc time: 679

;; 70
;; (totient 87109) => 79180 となるような数字の中で
;; (/ n (totient n))が最小となるnは?

;; (/ n (totient n)) が小さくなるのは(totient n)が大きい時
;; (totient n)が大きくなるのは素数、奇数。偶数は考えない。

(define p70-aux
  (lambda (ret min n m)
    (let ((toti (totient n)))
    (cond
      ((< m n) ret)
      ((= (list->integer (sort (integer->list toti) >))
          (list->integer (sort (integer->list n) >)))
       (if (< (/ n toti) min)
           (begin (println n) (p70-aux n (/ n toti) (+ n 2) m))
           (p70-aux ret min (+ n 2) m)))
      (else (p70-aux ret min (+ n 2) m))))))

(define p70
  (lambda (m)
    (p70-aux 0 +inf.0 3 m)))

;(time (p70 10000000))
;cpu time: 9609883 real time: 9649250 gc time: 12599
;これ以上思いつかない

;; 71
;; n/d (n<d)　について d<=1000000の分数を昇順に並べて3/7のすぐ左にある分数の分子は?

#|
;; ver1
;; 途中でとまった。流石に無理だった。
(define fractions_n/d
  (lambda (d)
    (flatten (map (lambda (de) (map (lambda (n) (/ n de)) (range 1 de))) (range 1 (+ d 1))))))

(define p71
  (lambda (d)
    (let ((ls (sort (fractions_n/d d) <)))
      (let ((i (index-of ls (/ 3 7))))
        (numerator (list-ref ls (- i 1)))))))
|#

;; ver2
(define p71
  (lambda (d)
    (define (aux ret d n D)
      (let ((n/d (/ n d)))
        (cond
          ((> d D) ret)
          ((< (/ 3 7) n/d) (aux ret (+ d 1) n D))
          ((and (< n/d (/ 3 7))
                (< ret n/d))
           (aux n/d d (+ n 1) D))
          (else (aux ret d (+ n 1) D)))))
    (numerator (aux 0 1 1 d))))

;(time (p71 1000000))  ;cpu time: 317 real time: 318 gc time: 22


;; 72
;; n/d (n<d) について d<=1000000の分数について約分できるものを除いた分数の数
;; 例 d = 10 の時 1/10,3/10,7/10, 9/10 -> 4 = (totient 10)

(define p72
  (lambda (d)
    (apply + (map totient (range 2 (+ d 1))))))

;(time (p72 1000000)) ;cpu time: 216830 real time: 217896 gc time: 12716

;; 73
;; n/d (n<d) について d≤12000の分数を昇順に並べた時 1/3と1/2の間にある分数の数。

;約分できるか判定
(define can-reduce?
  (lambda (n d)
    (not (= n (numerator (/ n d))))))

(define p73
  (lambda (d)
    (define (aux ret d n D)
      (let ((n/d (/ n d)))
        (cond
          ((> d D) ret)
          ((< 1/2 n/d) (aux ret (+ d 1) 1 D))
          ((and (< n/d 1/2)
                (< 1/3 n/d)
                (not (can-reduce? n d))) ;約分できるものは除く(重複になる)
           (aux (add1 ret) d (+ n 1) D))
          (else (aux ret d (+ n 1) D)))))
    (aux 0 2 1 d)))

;(time (p73 12000)) ;cpu time: 12834 real time: 12896 gc time: 605

;; 74
;; 1000000未満で 60チェーンでやっとループする数字は何個あるか
;; 69->363600->1454->169->363601(-> 1454)  5チェーンでループ

;69->363600
(define sum-digit-factorial
  (lambda (n)
    (apply + (map factorial (integer->list n)))))

;(make-chain 169 hash) -> '(169 363601 1454 169)
;(make-chain 69 hash) -> '(69 363600 1454 169 363601 1454)
;第2引数のhashにはそれまでに検証したnに対するチェーンの数が記録されている。
;例えば169は3つのチェーンだから(169 . 3)
;hashのなかに(169 . 3)があれば以下のようになる
;(make-chain 69 hash) -> '(69 363600 1454 (3))
(define make-chain
  (lambda (n h)
    (define (aux ret n h)
      (cond
        ((hash-has-key? h n) (append ret (list (list (hash-ref h n)))))
        ((member? n ret) (append ret (list n)))
        (else
         (aux (append ret (list n))
              (sum-digit-factorial n) h))))
    (aux '() n h)))


; h:ハッシュに各整数に対するチーン数を記録
; '(69 363600 1454 169 363601 1454)
; -> (69 . 5) (363600 . 4) (1454 . 3) (169 . 3) (363601 . 3)  
(define set-hash-chain
  (lambda (h ls)
    (define (aux h ls x c)
      (if (= (car ls) x)
          (map (lambda (n) (hash-set! h n c)) ls)
          (begin (hash-set! h (car ls) c)
                (aux h (cdr ls) x (- c 1)))))
    (if (list? (last ls))
        (aux h (drop-right ls 1)
             (last (drop-right ls 1))
             (sub1 (+ (length ls) (car (last ls)))))
        (aux h (drop-right ls 1)
             (last ls)
             (sub1 (length ls))))))

; nから0まででチェーン数がchainになるものをカウント
(define p74-aux
  (lambda (ret h n chain)
    (let ((c (make-chain n h)))
      (when (not (list? (car c))) (set-hash-chain h c))
      (cond
        ((zero? n) ret)
        ((= (hash-ref h n) chain)
         (p74-aux (add1 ret) h (sub1 n) chain))
        (else (p74-aux ret h (sub1 n) chain))))))

(define p74
  (lambda (n chain)
    (p74-aux 0 (make-hash) n chain)))

;(time (p74 1000000 60))  ;cpu time: 3518 real time: 3535 gc time: 915
    
;; 75
;; L <= 1,500,000  (L = a+b+c) において
;; L=12-> (a,b,c)=(3,4,5) のようにただ一つの整数の組み合わせで直角三角形を作れるLの個数


(define crat-aux
  (lambda (L c a)
    (cond
      ((< (- L c a) a) #f)
      ((= (+ (* a a) (* (- L c a) (- L c a))) (* c c)) #t)
      (else (crat-aux L c (+ a 1))))))
  
(define count-right-angle-triangle
  (lambda (L)
    (count (lambda (c) (crat-aux L c 1)) (range 3 (quotient L 2)))))

(define p75
  (lambda (L)
    (count (lambda (n) (= 1 (count-right-angle-triangle n))) (range L))))

;(time (p75 1500))
;んーーー

;; 76
;; 100を二つ以上の整数の合計として何通りの方法で表せれるか

;xについて x= a+b の時 a >= b となる組み合わせは何通りあるか?
(define count_>=
  (lambda (a b)
    (define (aux ret a b)
      (if (< a b)
          ret
          (aux (add1 ret) (- a 1) (+ b 1))))
    (aux 0 a b)))


(define count-p76-aux
  (lambda (a b n)
    (cond
      ((= 2 n) (if (>= a b)
                   (count_>= a b) 0))
      ((< a 1) 0)
      ((< a b) (+ (count-p76-aux a (+ 1 (- b a)) (sub1 n))
                  (count-p76-aux (- a 1) (+ b 1) n)))
      (else  (+ (count-p76-aux b 1 (sub1 n))
                (count-p76-aux (- a 1) (+ b 1) n))))))


(define count-p76
  (lambda (x n)
    (count-p76-aux (+ (- x n) 1) 1 n)))


;(define p76
;  (lambda (x)
;    (apply + (map (lambda (n) (count-p76 x n)) (range 2 x)))))

(define p76
  (lambda (x)
    (define (aux ret x n)
      (if (and (println n) (= n 1))
          ret
          (aux (+ ret (count-p76 x n)) x (- n 1))))
    (aux 0 x x)))

;(time (p76 100))
;cpu time: 693601 real time: 695912 gc time: 232

;; 77
;; 5000以上の通りの素数の和で表される数字

(define count-sum-primes-way
  (lambda (n)
    (combinations (primes_under n))))

;; 78

;; 79
;; ファイルからパスコードを予測する

;(read-lines ファイル名) p54

;同じ順番で要素を持っているか?
; (have-same-order? '(1 2 3) '(1 4 5 2 6 3)) -> #t
; (have-same-order? '(1 2 3) '(1 3 4 5 6 2)) -> #f
; (have-same-order? '(1 2 3) '(1 4 5 6 7 8)) -> #f
(define have-same-order?
  (lambda (l1 l2)
    (or (eqlist? l2 (filter (lambda (x) (member? x l2)) l1))
        (eqlist? l1 (filter (lambda (x) (member? x l1)) l2)))))

; lがllsの全てでhave-same-order?を満たすか
(define fulfill-list?
  (lambda (lls l)
    (cond
      ((null? lls) #t)
      ((have-same-order? (car lls) l)　(fulfill-list? (cdr lls) l))
      (else #f))))

; ファイルから候補(candidates)を作り出し
; candidatesの中から条件(requirements)を満たすものを選ぶ
(define p79
  (lambda ()
    (let ((requirements
           (map integer->list
                (map string->number
                     (read-lines "p079_keylog.txt")))))
       (let ((candidates (permutations
                          (list->set (flatten requirements)))))
         
         (list->integer
          (car　(filter (lambda (c) (fulfill-list? requirements c))
                        candidates)))))))
      
(time (p79))  ;cpu time: 69 real time: 70 gc time: 5
;; 一応できたけど、このやり方だと同じ数字を複数使われたパスコードは当てられない。

;; 80

;;開平法というのがあった
;;ルートで100桁求める

;都合により 文字列になった数字も可能にした
; (number-append 3 4) -> 34
; (number-append 3 "00") -> 300
(define number-append
  (lambda (n1 n2)
    (let ((sn1 (if (string? n1) n1
                   (number->string n1)))
          (sn2 (if (string? n2) n2
                   (number->string n2))))
      (string->number (string-append sn1 sn2)))))

;開平法に使う
;(1 2 3 4 5) -> '(1 23 45)
;(1 2 3 4) -> '(12 34)
;(1 2 3) -> '(1 23)
;(1 2) -> '(12)
(define list-append-2-number
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((null? (cdr ls)) ls)
      (else (append (list-append-2-number (drop-right ls 2))
                    (list (list->integer (take-right ls 2))))))))

; kaiheihouで使うNを求める処理
;x2の末尾にnをくっつけたx2nについて x2n * n がx1以下となる最大のnを求める
; (kaiheiho-get-N 143 4) -> 3 
(define kaiheiho-get-N
  (lambda (x1 x2)
    (define (aux x1 x2 n)
      (let ((xn (number-append x2 n)))
        (if (< x1 (* xn n))
            (- n 1)
            (aux x1 x2 (+ n 1)))))
    (aux x1 x2 0)))

; 開平法の計算法をそのまま
(define kaiheiho-aux
  (lambda (ret a b ls d)
    (let* ((bo (if (null? ls)
                  (number-append b "00")
                  (number-append b (car ls))))
           (n (kaiheiho-get-N bo a))
           (ao (number-append a n)))
      (cond
        ((= 1 d) ret)
        ((null? ls)
          (kaiheiho-aux (append ret (list n))
                        (+ ao n) (- bo (* ao n))
                        '() (sub1 d)))
        (else
          (kaiheiho-aux (append ret (list n))
                        (+ ao n) (- bo (* ao n))
                        (cdr ls) (sub1 d)))))))

; 開平法で ルートxをdigit桁まで求める
; (kaiheiho 2 6) -> '(1 4 1 4 2 1)
(define kaiheiho
  (lambda (x digit)
    (let* ((ls (list-append-2-number (integer->list x)))
           (is (integer-sqrt (car ls))))
      (kaiheiho-aux (list is) (* 2 is)
                    (- (car ls) (* is is))
                    (cdr ls) digit))))

;(apply + (kaiheiho 2 100)) ;475

(define p80
  (lambda (n digit)
    (define (aux ret n d)
      (cond
        ((zero? n) ret)
        ((integer? (sqrt n)) (aux ret (sub1 n) d))
        (else
         (aux (+ ret (apply + (kaiheiho n d)))
              (sub1 n) d))))
    (aux 0 n digit)))

(time (p80 100 100))
;cpu time: 641 real time: 649 gc time: 145

;; 81
;; 80*80 の数字について 左上から右下に移動する間の合計が最小となるのは?
#|
(define min-route
  (lambda (ls)
    (cond
      ((and (null? (cdr ls)) (null? (cdar ls))) (caar ls))
      ((null? (cdr ls)) (+ (caar ls) (min-route (list (cdar ls)))))
      ((null? (cdar ls)) (+ (caar ls) (min-route (cdr ls))))
      (else  (min (+ (caar ls) (min-route (cdr ls)))
                 (+ (caar ls) (min-route (map cdr ls))))))))


(define min81 100000000)
(define min-route
  (lambda (ls sum)
    (let ((sum (+ sum (caar ls))))
      (cond
        ((<= min81 sum) #f)
        ((and (null? (cdr ls)) (null? (cdar ls)))
         (when (< sum min81) (set! min81 sum)))
        ((null? (cdr ls)) (min-route (list (cdar ls)) sum))
        ((null? (cdar ls)) (min-route (cdr ls) sum))
        (else  (begin (min-route (cdr ls) sum)
                    (min-route (map cdr ls) sum)))))))

(define p81
  (lambda ()
    (let ((ls (map (lambda (s) (map string->number s))
                   (map (lambda (str) (string-split str ","))
                        (read-lines "p081_matrix.txt")))))
      (min-route ls 0) min81)))

(time (p81))
|#

;; 最短経路問題
;; ダイクストラ法アルゴリズム利用

;一次元ベクトルの要素の最小値
(define vector-min
  (lambda (vec)
    (apply min (vector->list vec))))

;二次元ベクトルの全ての要素の最小値
(define vect-min
  (lambda (vec)
    (apply min (filter-not (lambda (n) (or (eq? n -inf.0) (eq? n +inf.0)))
                           (flatten (map vector->list (vector->list vec)))))))

;二次元ベクトルのvector-member
;(vector-member 要素　ベクトル) -> #f or index
;(vec-member 要素　ベクトル) -> #f or '(y x)
(define vect-member
  (lambda (v vec)
    (define (aux v vec n)
      (let ((index (vector-member v (vector-ref vec n))))
        (cond
          ((eq? index #f)
           (if (zero? n) #f
               (aux v vec (- n 1))))
          (else (list n index)))))
    (aux v vec (sub1 (vector-length vec)))))

;二次元ベクトルのvector-set!
(define vect-set!
  (lambda (vec py px v)
    (vector-set! (vector-ref vec py) px v)))

;二次元ベクトルのvector-ref
(define vect-ref
  (lambda (vec py px)
    (vector-ref (vector-ref vec py) px)))

;ダイクストラ法
;2次元リストを引数　(list '(1 2 3) '(4 5 6) '(7 8 9))
;リストの各位置への最短経路のリストを返す。
;(list '(1 3 6) '(5 8 12) '(12 16 21))
;この場合 左上から右下の経路での和の最小値は21となる
(define dijkstra-aux
  (lambda (v v2 v3 y x)
    (let* ((oyox (vect-member (vect-min v2) v2))
           (oy (car oyox))
           (ox (cadr oyox))
           (oval (vect-ref v2 oy ox)))
      (vect-set! v3 oy ox oval)
      (vect-set! v2 oy ox -inf.0)
      (cond
        ((not (zero? (vect-ref v3 y x))) (vect-ref v3 y x))
        ((= oy y)
         (begin (when (< (+ oval (vect-ref v oy (+ ox 1))) (vect-ref v2 oy (+ ox 1)))
                  (vect-set! v2 oy (+ ox 1) (+ oval (vect-ref v oy (+ ox 1)))))
                (dijkstra-aux v v2 v3 y x)))
        ((= ox x)
         (begin (when (< (+ oval (vect-ref v (+ oy 1) x)) (vect-ref v2 (+ oy 1) ox))
                  (vect-set! v2 (+ oy 1) ox (+ oval (vect-ref v (+ oy 1) ox))))
                (dijkstra-aux v v2 v3 y x)))
        (else
         (begin (when (< (+ oval (vect-ref v (+ oy 1) x)) (vect-ref v2 (+ oy 1) ox))
                  (vect-set! v2 (+ oy 1) ox (+ oval (vect-ref v (+ oy 1) ox))))
                (when (< (+ oval (vect-ref v oy (+ ox 1))) (vect-ref v2 oy (+ ox 1)))        
                  (vect-set! v2 oy (+ ox 1) (+ oval (vect-ref v oy (+ ox 1)))))
                (dijkstra-aux v v2 v3 y x)))))))
         

(define dijkstra
  (lambda (ls)
    (let* ((lstype (map length ls))
           (l (map (lambda (li) (map (lambda (n) +inf.0) li)) ls))
           (v (list->vector (map list->vector ls)))
           (v2 (list->vector (map list->vector l)))
           (v3 (list->vector (map make-vector lstype))))
      (vect-set! v2 0 0 (caar ls))
      (dijkstra-aux v v2 v3 (sub1 (length lstype)) (sub1 (last lstype))))))


;(define p81
;  (lambda ()
;    (let ((ls (map (lambda (s) (map string->number s))
;                   (map (lambda (str) (string-split str ","))
;                        (read-lines "p081_matrix.txt")))))
;      (dijkstra ls))))

;(time (p81))  ;cpu time: 1680 real time: 1697 gc time: 151

;;Dijkstra 一般化
(define D-aux-aux
  (lambda (v v2 v3 oy ox col row oval ways)
    (for ([way ways])
      (let ((y (+ oy (car way)))
            (x (+ ox (cadr way))))
        (when (and (<= 0 y) (<= 0 x) (< y col) (< x row)
                   (< (+ oval (vect-ref v y x)) (vect-ref v2 y x)))
          (vect-set! v2 y x (+ oval (vect-ref v y x))))))))

(define Dijkstra-aux
  (lambda (v v2 v3 gy gx col row ways)
    (let* ((oyox (vect-member (vect-min v2) v2))
           (oy (car oyox))
           (ox (cadr oyox))
           (oval (vect-ref v2 oy ox)))
      (vect-set! v3 oy ox oval)
      (vect-set! v2 oy ox -inf.0)
      (if (not (zero? (vect-ref v3 gy gx)))
          (vect-ref v3 gy gx)
          (begin (D-aux-aux v v2 v3 oy ox col row oval ways)
                 (Dijkstra-aux v v2 v3 gy gx col row ways))))))


;ls 最短経路を見つけたいリスト(n*m)
;sy,sx はスタート地点のy座標,x座標
;gy gx はゴール地点のy座標,x座標
;ways は経路の進み方を以下のように指定
;右 (0 1)
;左 (0 -1)
;上 (1 0)
;下 (-1 0)
;右斜め上 (1 1)  *斜めは上下左右の組み合わせで
;全方向に進む場合は指定なしでいい

(define Dijkstra
  (lambda (ls sy sx gy gx . ways)
    (let* ((col (length ls))
           (row (length (car ls)))
           (ls2 (map (lambda (li) (map (lambda (n) +inf.0) li)) ls))
           (ls3 (map (lambda (li) (map (lambda (n) 0) li)) ls))
           (v (list->vector (map list->vector ls)))
           (v2 (list->vector (map list->vector ls2)))
           (v3 (list->vector (map list->vector ls3))))
      (vect-set! v2 sy sx (vect-ref v sy sx))
      (when (null? ways)
        (set! ways (list '(0 1) '(0 -1) '(1 0) '(-1 0)
                         '(1 1) '(1 -1) '(-1 1) '(-1 -1))))
      (Dijkstra-aux v v2 v3 gy gx col row ways))))

(define p81
  (lambda ()
    (let ((ls (map (lambda (s) (map string->number s))
                   (map (lambda (str) (string-split str ","))
                        (read-lines "p081_matrix.txt")))))
      (println (caar ls))
      (Dijkstra ls 0 0 79 79 '(0 1) '(1 0)))))

;(time (p81))  ;cpu time: 2494 real time: 2524 gc time: 826


(Dijkstra (list (list 131 673 234 103 18)
                (list 201 96 342 965 150)
                (list 630 803 746 422 111)
                (list 537 699 497 121 956)
                (list 805 732 524 37 331))
          0 0 4 4 '(0 1) '(1 0))


;; 82


;;Dijkstra 複数のゴールに対応

(define Dijkstra*-finish?
  (lambda (v3 goals)
    (cond
      ((null? goals) #t)
      ((zero? (vect-ref v3 (caar goals) (cadar goals))) #f)
      (else (Dijkstra*-finish? v3 (cdr goals))))))

(define Dijkstra*-aux
  (lambda (v v2 v3 goals col row ways)
    (let* ((oyox (vect-member (vect-min v2) v2))
           (oy (car oyox))
           (ox (cadr oyox))
           (oval (vect-ref v2 oy ox)))
      (vect-set! v3 oy ox oval)
      (vect-set! v2 oy ox -inf.0)
      (if (Dijkstra*-finish? v3 goals)
          (for/list ([gygx goals])
            (vect-ref v3 (car gygx) (cadr gygx)))
          (begin (D-aux-aux v v2 v3 oy ox col row oval ways)
                 (Dijkstra*-aux v v2 v3 goals col row ways))))))

;; Dijkstraの複数ゴールver
;; goals '((gy1 gx1) (gy2 gx2) ... (gyn gxn))
;; それぞれのゴールへの最短経路がリストで得られる
;; '(value1 value2 value3 ... valuen)

(define Dijkstra*
  (lambda (ls sy sx goals . ways)
    (let* ((col (length ls))
           (row (length (car ls)))
           (ls2 (map (lambda (li) (map (lambda (n) +inf.0) li)) ls))
           (ls3 (map (lambda (li) (map (lambda (n) 0) li)) ls))
           (v (list->vector (map list->vector ls)))
           (v2 (list->vector (map list->vector ls2)))
           (v3 (list->vector (map list->vector ls3))))
      (vect-set! v2 sy sx (vect-ref v sy sx))
      (when (null? ways)
        (set! ways (list '(0 1) '(0 -1) '(1 0) '(-1 0)
                         '(1 1) '(1 -1) '(-1 1) '(-1 -1))))
      (Dijkstra*-aux v v2 v3 goals col row ways))))

(define p82
  (lambda ()
    (let* ((ls (map (lambda (s) (map string->number s))
                   (map (lambda (str) (string-split str ","))
                        (read-lines "p082_matrix.txt"))))
           (goals (map (lambda (y) (list y 79)) (range 80)))
           (l (for/list ([sy (range 80)])
                (Dijkstra* ls sy 0 goals '(0 1) '(1 0) '(-1 0)))))
      (apply min (flatten l)))))
        
;(time (p82))  ;cpu time: 143934 real time: 140746 gc time: 15878


;; 83
(define p83
  (lambda ()
    (let ((ls (map (lambda (s) (map string->number s))
                   (map (lambda (str) (string-split str ","))
                        (read-lines "p083_matrix.txt")))))
      (Dijkstra ls 0 0 79 79 '(0 1) '(0 -1) '(1 0) '(-1 0)))))
        
;(time (p83))  ;cpu time: 1716 real time: 1726 gc time: 160

;; 85
;; 横をx個　縦をy個としたら四角の数は
;; Σ(1->x)n * Σ(1->y)n　になってる

(define sum-over
  (lambda (n)
    (define (aux x n)
      (if (<= n 0)
          (- x 1)
          (aux (+ x 1) (- n x))))
    (aux 1 n)))

(define p85-aux
  (lambda (max-x n)
    (let* ((xsum 0)
           (ysum 0)
           (d (abs (- n xsum)))
           (ret 0))
      (for ([x (range 1 max-x)])
        (set! xsum (+ xsum x))
        (for ([y (range 1 max-x)])
          (set! ysum (+ ysum y))
          (let ((d0 (abs (- n (* xsum ysum)))))
            (when (< d0 d) (begin (set! d d0) (set! ret (* x y))))))
        (set! ysum 0))
      ret)))

(define p85
  (lambda (n)
    (let ((max-x (sum-over n)))
      (p85-aux max-x n))))

(time (p85 2000000))  ;cpu time: 231 real time: 231 gc time: 0

;; 87

;; index乗して　n以下となる素数のリストを返す
(define prime-pow-unders
  (lambda (n index)
    (define (aux ret x index n)
      (cond
        ((< n (** x index)) ret)
        ((prime? x) (aux (append ret (list x)) (+ x 1) index n))
        (else (aux ret (+ x 1) index n))))
    (aux '() 2 index n)))


;; 上と役割は同じだけど
;; ls の要素の中でindex乗がn以下となる要素だけを返す
(define pow-unders
  (lambda (n index ls)
    (define (aux ret index n ls)
      (if (null? ls)
          ret
          (let ((x (** (car ls) index)))
            (if (< n x)
                ret
                (aux (cons x ret) index n (cdr ls))))))
    (aux '() index n ls)))

(define p87-aux
  (lambda (l1 i1 l2 i2 n)
    (define (aux ret l1 i1 l2 i2 n)
       (if (null? l1)
           ret
           (let ((x (** (car l1) i1)))
             (if (< n x)
                 ret
                 (aux (append ret (map (lambda (a) (+ a x))
                                       (pow-unders (- n x) i2 l2)))
                      (cdr l1) i1 l2 i2 n)))))
    (aux '() l1 i1 l2 i2 n)))

(define p87
  (lambda (n)
    (let ((l2 (prime-pow-unders n 2))
          (l3 (prime-pow-unders n 3))
          (l4 (prime-pow-unders n 4)))
      (let ((ls (for/list ([p l2])
                  (let ((x (** p 2)))
                    (map (lambda (a) (+ a x))
                         (p87-aux l3 3 l4 4 (- n x)))))))
        (length (remove-duplicates (flatten ls)))))))

;(time (p87 50000000))
;cpu time: 1177 real time: 1186 gc time: 288


;; 89

(define roman (hash "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000))
(define roman->integer
  (lambda (r) r))
(define integer->roman
  (lambda (x) x))
;(read-lines "p089_roman.txt")

;; 91

(define p91
  (lambda (x y)
    (define (aux ret x1 y1 x y)
      (for ([x2 (range 0 (+ x 1))])
        (for ([y2 (range 0 (+ y 1))])
          (let ((a (+ (** x1 2) (** y1 2)))
                (b (+ (** x2 2) (** y2 2)))
                (c (+ (** (- x1 x2) 2) (** (- y1 y2) 2))))
            (when (and (not (and (zero? x2) (zero? y2)))
                       (not (and (= x1 x2) (= y1 y2)))
                       (or (= (+ a b) c)
                           (= (+ a c) b)
                           (= (+ b c) a)))
               (set! ret (+ ret 1))))))
      (if (and (= x1 x) (= y1 y))
          (/ ret 2)
          (if (= x1 x)
              (aux ret 0 (+ y1 1) x y)
              (aux ret (+ x1 1) y1 x y))))
    (aux 0 1 0 x y)))

;(time (p91 50 50)) ;cpu time: 2811 real time: 2822 gc time: 0
    
;; 92

(define arrive89?
  (lambda (n h)
    (define (aux chain n h)
      (let* ((l (sort (integer->list n) >))
             (next (apply + (map (lambda (a) (* a a)) l)))
             (k (list->integer l)))
        (cond
          ((hash-has-key? h k)
           (let ((x (hash-ref h k)))
             (for ([c chain])
               (hash-set! h c x))
             (eq? x 89)))
          ((or (= n 1) (= n 89))
           (begin (for ([c (cons k chain)])
                    (hash-set! h c n))
                  (eq? n 89)))
          (else
           (aux (cons k chain) next h)))))
    (aux '() n h)))

(define p92-aux
  (lambda (ret n h)
    (cond
      ((zero? n) ret)
      ((arrive89? n h) (p92-aux (add1 ret) (- n 1) h))
      (else (p92-aux ret (- n 1) h)))))

(define p92
  (lambda (n)
    (let ((h (make-hash)))
      (p92-aux 0 n h))))

;(time (p92 10000000))
;cpu time: 27573 real time: 27815 gc time: 4267

;; 96
;; SuDoku

; ある位置 (x,y)について、その縦列/横列/(x,y)含む9マスの四角形に
; 出現する数字のリストを返す。
(define sudoku-candidate-aux
  (lambda (vec x y)
    (let ((col (vector->list (vector-ref vec y)))
          (row (for/list ([i (range 9)])
                 (vect-ref vec i x))))
      (let* ((xo (cond
                   ((< x 3) 0)
                   ((> x 5) 6)
                   (else 3)))
             (yo (cond
                   ((< y 3) 0)
                   ((> y 5) 6)
                   (else 3)))
             (box (for/list ([i (range yo (+ yo 3))])
                    (for/list ([j (range xo (+ xo 3))])
                      (vect-ref vec i j)))))
        (filter-not zero? (flatten (append col row box)))))))

; ある位置(x,y)について、ありうる数字のリストを返す。
(define sudoku-candidate
  (lambda (vec x y)
    (let ((ls (sudoku-candidate-aux vec x y)))
      (filter-not (lambda (n) (member? n ls)) (range 1 10)))))

; 一つ数字を仮定して(sudoku)を解く。
(define sudoku-hard-aux
  (lambda (vec x y ls)
    (if (null? ls)
        #f
        (begin (vect-set! vec y x (car ls))
               (let ((result (sudoku (map vector->list (vector->list vec)))))
                 (if (not (eq? result #f))
                     result
                     (sudoku-hard-aux vec x y (cdr ls))))))))

;次考える(x,y)を返す
(define next-xy
  (lambda (x y)
    (if (= x 8)
        (list 0 (+ y 1))
        (list (+ x 1) y))))    


; 難解な数独を解く
(define sudoku-hard
  (lambda (ls)
    (define (aux vec x y)
      (if (not (zero? (vect-ref vec y x)))
          (aux vec (car (next-xy x y)) (cadr (next-xy x y)))
          (let ((ls (sudoku-candidate vec x y)))
            (sudoku-hard-aux vec x y ls))))
    (let ((vec (list->vector (map list->vector ls))))
      (aux vec 0 0))))


; 簡単な数独(確定する部分を順番に埋めれば解ける数独)を解く。
; 難しい数独(確定する部分を順番に埋め他だけでは解けない数独)は(sudoku-hard)に処理を移す。
(define sudoku-aux
  (lambda (vec)
     (let* ((hard-to-solve? #t)
            (solved? #t)
            (can-be-solve? #t))
       (for ([y (range 9)])
         (for ([x (range 9)])
           (when (zero? (vect-ref vec y x))
             (set! solved? #f)
             (let ((l (sudoku-candidate vec x y)))
               (when (null? l)
                 (set! can-be-solve? #f))
               (when (= (length l) 1)
                 (set! hard-to-solve? #f)
                 (vect-set! vec y x (car l)))))))
       (cond
         (solved? vec)
         ((not can-be-solve?) #f)
         (hard-to-solve? (sudoku-hard (map vector->list (vector->list vec))))
         (else (sudoku-aux vec))))))

;; 数独を解く 戻り値は#(#(...)#(...) #...) 形式
(define sudoku
  (lambda (ls)
    (let ((vec (list->vector (map list->vector ls))))
      (sudoku-aux vec))))

;複数の数独を解き、結果をベクトルのリストで返す。
(define p96-aux
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (sudoku (take (cdr ls) 9))
           (p96-aux (drop ls 10))))))

; ファイルから数独問題を解ける形に変形してauxに渡す。
(define p96
  (lambda ()
    (let ((lines (read-lines "p096_sudoku.txt")))
      (let ((result (p96-aux (map (lambda (l) (map (lambda (c) (- (char->integer c) 48)) l))
                                  (map string->list lines)))))
        (apply + (map list->integer
                    (map (lambda (l) (take l 3))
                         (map vector->list
                              (map car
                                   (map vector->list
                                        result))))))))))

(time (p96))
;cpu time: 891 real time: 902 gc time: 228

;; p97
;; 28433 x 2^7830457 +1 の下10桁

(define pow2-last-10digit
  (lambda (n)
    (define (aux x n)
      (if (zero? n)
          x
          (let ((x*2 (* x 2)))
            (if (> x*2 9999999999)
                (aux (- x*2 10000000000) (sub1 n))
                (aux x*2 (sub1 n))))))
    (aux 1 n)))

(define p97
  (lambda ()
    (let ((x (+ (* 28433 (pow2-last-10digit 7830457)) 1)))
      (list->integer (take-right (integer->list x) 10)))))

(time (p97))
;cpu time: 221 real time: 222 gc time: 0

;; 99

;; '((519432 525806) (632382 518061) ... (a,b)...)
;; 上のようなリストに対しa^bが最大となる位置を返す。
;; (** 519432 525806) の代わりに
;; (* 525806 (log 519432)) で大小を比べる

(define get-index-max-pow
  (lambda (lls)
    (define (aux ret max lls n)
      (cond
        ((null? lls) ret)
        ((< max (* (cadar lls) (log (caar lls))))
         (aux n (* (cadar lls) (log (caar lls)))
              (cdr lls) (+ n 1)))
        (else (aux ret max (cdr lls) (+ n 1)))))
    (aux 0 0 lls 1)))

(define p99
  (lambda ()
    (let* ((lines (read-lines "p099_base_exp.txt"))
           (ls (map (lambda (s) (map string->number
                                     (string-split
                                      (string-replace s "," " "))))
                    lines)))
      (get-index-max-pow ls))))

(time (p99))
;cpu time: 19 real time: 19 gc time: 12

;; 100
;; 箱に青15個と赤6個のボールが入っている。
;; 2つのボールをとる時2つとも青をとる確率は (15/21)*(14/20)=1/2
;; 青85個、赤35個の時も1/2になる。
;; ボールが合計10^12個以上入ってる時に1/2となる青の個数は?



;; 112
;bouncy numbers の割合が99%になるn

;(define bouncy-number?
;  (lambda (x)
;    (let ((ls (integer->list x)))
;      (if (or (eqlist? ls (sort ls <))
;              (eqlist? ls (sort ls >)))
;          #f
;          #t))))

(define bn-aux
  (lambda (ls comp)
    (cond
      ((null? (cdr ls)) #f)
      ((or (= (first ls) (second ls))
           (comp (first ls) (second ls)))
       (bn-aux (cdr ls) comp))
      (else #t))))


(define bouncy-number?
  (lambda (x)
    (let* ((ls (integer->list x))
           (f (car ls))
           (l (last ls)))
      (cond
        ((= f l)
         (if (null? (cdr (remove-duplicates ls)))
             #f
             #t))
        ((< f l) (bn-aux ls <))
        (else (bn-aux ls >))))))

(define p112-aux
  (lambda (pct bn eln x)
    (if (= pct (/ bn (+ bn eln)))
        (- x 1)
        (if (bouncy-number? x)
            (p112-aux pct (add1 bn) eln (+ x 1))
            (p112-aux pct bn (add1 eln) (+ x 1))))))

(define p112
  (lambda (pct)
    (p112-aux (/ pct 100) 0 1 2)))

;(time (p112 99))
;cpu time: 1543 real time: 1560 gc time: 219

;; 120


;(define test
;  (lambda (a)
;    (for/list ([n (range 1 50)])
;      (modulo (+ (** (- a 1) n) (** (+ a 1) n)) (* a a)))))

;(test 3) -> n=1,7,13の時最大6    -> a*2
;(test 4) -> n=1,3,5の時最大8     -> a*2
;(test 5) -> n=7,17,27の時最大20  -> a*4
;(test 6) -> n=5,11,17の時最大24  -> a*4
;(test 7) -> n=3,17,31の時最大42  -> a*6
;(test 8) -> n=3,7,11の時最大48   -> a*6
;(test 9) -> n=13    の時最大72   -> a*8
;(test 10) -> n=9    の時最大80   -> a*8

(define p120
  (lambda ()
    (apply + (map (lambda (a) (if (odd? a)
                                  (* a (- a 1))
                                  (* a (- a 2))))
                  (range 3 1001)))))

(time (p120))
;cpu time: 0 real time: 1 gc time: 0

;; 125
;; 10^8 以下の回文数時のうち、連続する数字の2乗の和として表されるものの合計

;(palindromic_number? n) p4で定義

(define spacs-aux
  (lambda (ret num x n)
    (cond
      ((< n num) ret)
      ((palindromic_number? num)
       (spacs-aux (cons num ret) (+ num (* x x)) (+ x 1) n))
      (else (spacs-aux ret (+ num (* x x)) (+ x 1) n)))))

(define sum-prindromic-and-consecutive-squares
  (lambda (ret x n)
    (if (< n (* x x))
        (apply + (remove-duplicates ret))
        (sum-prindromic-and-consecutive-squares
         (append ret (spacs-aux '()
                                (+ (* x x) (* (+ x 1) (+ x 1)))
                                (+ x 2) n))
         (+ x 1) n))))

(define p125
  (lambda (n)
    (sum-prindromic-and-consecutive-squares '() 1 n)))

(time (p125 100000000))
;cpu time: 207 real time: 212 gc time: 93

;; 145
;; 10^9以下のリバーシブルナンバーはいくつあるか?
;; リバーシブルナンバー : n + reverse(n) = 奇数のみで構成された数字
;; 100のように最後に0がつく数字は考えない
(define all_digit_odd?
  (lambda (x)
    (define (aux ls)
      (if (null? ls)
          #t (if (odd? (car ls))
                 (aux (cdr ls)) #f)))
    (aux (integer->list x))))

(define p145-aux
  (lambda (ret n x)
    (cond
      ((= n x) ret)
      ((zero? (modulo n 10)) (p145-aux ret (+ n 1) x))
      (else
       (let* ((ls (integer->list n))
              (rls (reverse ls))
              (rn (list->integer rls))
              (num (+ n rn)))
         (cond
           ((< rn n) (p145-aux ret (+ n 1) x))
           ((all_digit_odd? num)
            (if (= rn n)
                (p145-aux (+ ret 1) (+ n 1) x)
                (p145-aux (+ ret 2) (+ n 1) x)))
           (else (p145-aux ret (+ n 1) x))))))))
                             
(define p145
  (lambda (n)
    (p145-aux 0 2 n)))

;(time (p145 1000000000))
;cpu time: 1384230 real time: 1401021 gc time: 69751