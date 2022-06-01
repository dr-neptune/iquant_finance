#lang racket
(require racket)

;; Interest rates and compounding

#<<|
  Calculates the growth of the initial deposit (notional)
  notional: initial deposit
  interest-rate: the static interest rate over the time period
  compounding: the number of times the balance compounds per year
  maturity: the number of years to compound
|
(define (static-interest-rate notional interest-rate compounding-period maturity)
  (* notional
     (expt (add1 (/ interest-rate compounding-period))
           (* compounding-period maturity))))

(static-interest-rate 100 0.05 1 10)

;; Zero coupon bonds and discounting

#<<|
Calculates the value of a zero coupon bond given the
continuously (daily) compounded interest rate from t until T
t1: integer: the starting time period
t2: integer: the ending time period
interest-rate: float: the static interest rate for the time period
|
(define (cts-zero-coupon-bond-price t1 t2 interest-rate)
  (exp (- (* interest-rate (- t2 t1)))))

(cts-zero-coupon-bond-price 0 10 0.05)


(define (annual-zero-coupon-bond-price t1 t2 interest-rate)
  (/ 1 (expt (add1 interest-rate) (- t2 t1))))

(annual-zero-coupon-bond-price 0 1 0.05)


(define (discount-factor cashflow-amt t1 t2 interest-rate)
  (* cashflow-amt (cts-zero-coupon-bond-price t1 t2 interest-rate)))

(discount-factor 1000 0 10 0.01)


;; annuities

(define (annuity-value cashflow-amt t1 t2 zcb-fn interest-rate)
  (* cashflow-amt
     (foldl + 0 (map (λ (x) (zcb-fn t1 x interest-rate))
                     (inclusive-range t1 t2)))))

(annuity-value 1000 1 3 annual-zero-coupon-bond-price 0.05)


(define (annual-annuity-value-closed-form cashflow-amt t1 t2 interest-rate)
  (+ cashflow-amt
     (* cashflow-amt
        (* (/ 1 interest-rate)
           (- 1 (/ 1 (expt (+ 1 interest-rate) (- t2 (sub1 t1)))))))))

(annuity-value-closed-form 1000 1 2 0.05)

;; forward zero coupon bond prices
(define (forward-zcb-price t0 t1 t2 interest-rate zcb-fn)
  (/ (zcb-fn t0 t1 interest-rate)
     (zcb-fn t0 t2 interest-rate)))

(forward-zcb-price 0 5 10 0.05 annual-zero-coupon-bond-price)
