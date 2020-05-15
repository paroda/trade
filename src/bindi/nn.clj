(ns bindi.nn
  (:require [uncomplicate.commons.core
             :refer [with-release let-release Releaseable release]]
            [uncomplicate.neanderthal
             [core :refer [mrows ncols dim raw view view-ge vctr copy row
                           entry! axpby! axpy! copy! scal! mv! transfer! transfer
                           mm! rk! ge trans nrm2 zero]]
             [math :refer [sqrt log sin pi sqr]]
             [vect-math :refer [tanh! linear-frac! sqr! mul! cosh! inv!]]
             [native :refer [fge native-float]]]
            [uncomplicate.fluokitten.core :refer [fmap!]])
  (:import clojure.lang.IFn))

(defprotocol Parameters
  (weights [this])
  (bias [this]))

(defprotocol ActivationProvider
  (activation-fn [this]))

(deftype FullyConnectedInference [w b activ-fn]
  Releaseable
  (release [_]
    (release w)
    (release b)
    (release activ-fn))
  Parameters
  (weights [_] w)
  (bias [_] b)
  ActivationProvider
  (activation-fn [_] (activ-fn))
  IFn
  (invoke [_ x ones a]
    (activ-fn (rk! -1.0 b ones (mm! 1.0 w x 0.0 a)))))

(defn fully-connected
  ([factory in-dim out-dim activ-fn]
   (let-release [w (ge factory out-dim in-dim)
                 b (vctr factory out-dim)]
     (->FullyConnectedInference w b activ-fn)))
  ([out-dim activ-fn]
   (fn
     ([factory in-dim]
      (fully-connected factory in-dim out-dim activ-fn))
     ([]
      out-dim))))

(defprotocol Backprop
  (forward [this])
  (backward [this eta]))

(defprotocol Transfer
  (input [this])
  (output [this])
  (ones [this]))

(defprotocol Activation
  (activ [_ z a!])
  (prime [_ z!]))

(deftype FullyConnectedTraining [v w b a-1 z a ones activ-fn first?]
  Releaseable
  (release [_]
    (release v)
    (release w)
    (release b)
    (release a-1)
    (release z)
    (release a)
    (release ones)
    (release activ-fn))
  Parameters
  (weights [_] w)
  (bias [_] b)
  Transfer
  (input [_] a-1)
  (output [_] a)
  (ones [_] ones)
  Backprop
  (forward [_]
    (activ activ-fn (rk! -1.0 b ones (mm! 1.0 w a-1 0.0 z)) a))
  (backward [_ [eta lambda mu]]
    (let [eta-avg (- (/ (double eta) (dim ones)))]
      (mul! (prime activ-fn z) a)
      (mm! eta-avg z (trans a-1) mu v)
      (when-not first? (mm! 1.0 (trans w) z 0.0 a-1))
      (mv! eta-avg z ones 1.0 b)
      (axpby! 1.0 v (inc (* eta-avg (double lambda))) w))))

(defn training-layer
  ([inference-layer input ones-vctr first?]
   (let-release [w (view (weights inference-layer))
                 v (zero w)
                 b (view (bias inference-layer))
                 a-1 (view input)
                 z (ge w (mrows w) (dim ones-vctr))
                 a (raw z)
                 o (view ones-vctr)]
     (->FullyConnectedTraining v w b a-1 z a o ((activation-fn inference-layer) z) first?)))
  ([inference-layer input ones-vctr]
   (training-layer inference-layer input ones-vctr true))
  ([inference-layer previous-backprop]
   (training-layer inference-layer
                   (output previous-backprop)
                   (ones previous-backprop)
                   false)))

(deftype SigmoidActivation [work]
  Releaseable
  (release [_]
    (release work))
  Activation
  (activ [_ z a!]
    (linear-frac! 0.5 (tanh! (scal! 0.5 (copy! z a!))) 0.5))
  (prime [this z!]
    (linear-frac! 0.5 (tanh! (scal! 0.5 z!)) 0.5)
    (mul! z! (linear-frac! -1.0 z! 1.0 work))))

(defn sigmoid
  ([]
   (fn [z]
     (let-release [work (raw z)]
       (->SigmoidActivation work))))
  ([z!]
   (linear-frac! 0.5 (tanh! (scal! 0.5 z!)) 0.5)))

(deftype TanhActivation []
  Activation
  (activ [_ z a!]
    (tanh! z a!))
  (prime [this z!]
    (sqr! (inv! (cosh! z!)))))

(defn tanh
  ([]
   (fn [_] (->TanhActivation)))
  ([z!]
   (tanh! z!)))

(deftype LinearActivation []
  Activation
  (activ [_ z a!]
    (copy! z a!))
  (prime [this z!]
    (entry! z! 1)))

(defn linear
  ([]
   (fn [_] (->LinearActivation)))
  ([z!]
   z!))

(defprotocol NeuralNetwork
  (layers [this]))

(deftype NeuralNetworkInference [layers
                                 ^long max-width-1
                                 ^long max-width-2]
  Releaseable
  (release [_]
    (doseq [l layers] (release l)))
  NeuralNetwork
  (layers [_]
    layers)
  IFn
  (invoke [_ x ones-vctr temp-1! temp-2!]
    (let [batch (dim ones-vctr)]
      (loop [x x v1 temp-1! v2 temp-2! layers layers]
        (if layers
          (recur (let [layer (first layers)]
                   (layer x ones-vctr
                          (view-ge v1 (mrows (weights layer)) batch)))
                 v2 v1 (next layers))
          x))))
  (invoke [this x a!]
    (let [cnt (count layers)]
      (if (= 0 cnt)
        (copy! x a!)
        (with-release [ones-vctr (entry! (vctr x (ncols x)) 1.0)]
          (if (= 1 cnt)
            ((layers 0) x ones-vctr a!)
            (with-release [temp-1 (vctr x (* max-width-1 (dim ones-vctr)))]
              (if (= 2 cnt)
                (this x ones-vctr temp-1 a!)
                (with-release [temp-2 (vctr x (* max-width-2 (dim ones-vctr)))]
                  (copy! (this x ones-vctr temp-1 temp-2) a!)))))))))
  (invoke [this x]
    (let-release [a (ge x (mrows (weights (peek layers))) (ncols x))]
      (this x a))))

(defn inference-network [factory in-dim layers]
  (let [out-sizes (map #(%) layers)
        in-sizes (cons in-dim out-sizes)
        max-width-1 (apply max (take-nth 2 out-sizes))
        max-width-2 (apply max (take-nth 2 (rest out-sizes)))]
    (let-release [layers (vec (map (fn [layer-fn in-size]
                                     (layer-fn factory in-size))
                                   layers
                                   in-sizes))]
      (->NeuralNetworkInference layers max-width-1 max-width-2))))

(deftype NeuralNetworkTraining [forward-layers backward-layers]
  Releaseable
  (release [_]
    (doseq [l forward-layers] (release l)))
  NeuralNetwork
  (layers [_]
    forward-layers)
  Transfer
  (input [_] (input (first forward-layers)))
  (output [_] (output (first backward-layers)))
  (ones [_] (ones (first backward-layers)))
  Backprop
  (forward [_]
    (doseq [layer forward-layers]
      (forward layer))
    (output (first backward-layers)))
  (backward [_ eta]
    (doseq [layer backward-layers]
      (backward layer eta))))

(defn training-network [inference input]
  (let-release [ones-vctr (entry! (raw (row input 0)) 1)]
    (let-release [backward-layers
                  (reduce (fn [bwd-layers layer]
                            (cons (training-layer layer (first bwd-layers))
                                  bwd-layers))
                          (list (training-layer (first (layers inference)) input ones-vctr))
                          (rest (layers inference)))]
      (->NeuralNetworkTraining (reverse backward-layers) backward-layers))))

(defn sgd
  ([network out cost! epochs eta-lambda-mu]
   (dotimes [n epochs]
     (forward network)
     (cost! out (output network))
     (backward network eta-lambda-mu))
   (cost! (output network)))
  ([network out cost! options]
   (map (fn [[epochs eta-lambda-mu]]
          (sgd network out cost! epochs eta-lambda-mu))
        options)))

(defn quadratic-cost!
  ([y-a]
   (/ (sqr (nrm2 y-a)) (* 2 (dim y-a))))
  ([y a!]
   (axpy! -1.0 y a!)))

(defn rand-uniform ^double [^double _]
  (double (rand)))

(defn rand-gaussian ^double [^double _]
  (let [u1 (rand-uniform Double/NaN)
        u2 (rand-uniform Double/NaN)]
    (double (* (sqrt (* -2.0 (log u1))) (sin (* 2.0 pi u2))))))

(defn init-layer! [layer!]
  (let [w (weights layer!)]
    (scal! (sqrt (/ 2.0 (+ (mrows w) (ncols w)))) (fmap! rand-gaussian w))
    (fmap! rand-gaussian (bias layer!))
    layer!))

(defn init! [network!]
  (doseq [layer (layers network!)]
    (init-layer! layer))
  network!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (rand-uniform Double/NaN)
  (rand-gaussian Double/NaN)

  (with-release [x (ge native-float 2 2)]
    (fmap! rand-uniform x)
    (transfer x))

  (with-release [x (ge native-float 2 2 [0.3 0.9 0.3 0.9])
                 a (ge native-float 1 2)
                 inference (inference-network native-float 2
                                              [(fully-connected 4 tanh)
                                               (fully-connected 1 sigmoid)])
                 layers (.layers ^NeuralNetworkInference inference)]
    (transfer! [0.3 0.1 0.9 0.0 0.6 2.0 3.7 1.0] (weights (layers 0)))
    (transfer! [0.7 0.2 1.1 2] (bias (layers 0)))
    (transfer! [0.75 0.15 0.22 0.33] (weights (layers 1)))
    (transfer! [0.3] (bias (layers 1)))
    (transfer (inference x a)))

  (with-release [x (ge native-float 2 2 [0.3 0.9 0.3 0.9])
                 y (ge native-float 1 2 [0.50 0.50])
                 inference (inference-network
                            native-float 2
                            [(fully-connected 4 tanh)
                             (fully-connected 1 sigmoid)])
                 inf-layers (layers inference)
                 training (training-network inference x)]
    (transfer! [0.3 0.1 0.9 0.0 0.6 2.0 3.7 1.0] (weights (inf-layers 0)))
    (transfer! [0.7 0.2 1.1 2] (bias (inf-layers 0)))
    (transfer! [0.75 0.15 0.22 0.33] (weights (inf-layers 1)))
    (transfer! [0.3] (bias (inf-layers 1)))
    {:untrained (transfer (inference x))
     :cost [(sgd training y quadratic-cost! 1 [0.05 0.1 0.1])
            (sgd training y quadratic-cost! 20 [0.05 0.1 0.1])
            (sgd training y quadratic-cost! 200 [0.05 0.1 0.1])
            (sgd training y quadratic-cost! 2000 [0.05 0.1 0.1])]
     :trained (transfer (inference x))
     :messed-up-inputs (transfer x)})

  )
