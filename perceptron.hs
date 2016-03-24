type DataPoint  a = ([a],a)
data Neuron a = Neuron ([a]) deriving Show

--Return the dot product of Neuron a with  
neuron_fire :: (Num a) => Neuron a -> [a] -> a
neuron_fire (Neuron (bias:ws)) xs = (foldr (+) bias (zipWith (*)  ws xs))

--Return a better neuron, updated with the new point
better_neuron :: (Num a) => a -> DataPoint a -> Neuron a -> Neuron a
better_neuron rate (xs, val) neuron@(Neuron weights)=
    let response = neuron_fire neuron xs
        adjustments = [rate * (val - response) * x | x<-(1:xs)]
        new_weights = zipWith (+) weights  adjustments
        in (Neuron new_weights)
--Return a neuron that seperates the given data
--Loops infinitely if the data is not linearly separable
train_ :: (Ord a, Num a) => Neuron a -> a ->  [DataPoint a] -> Neuron a
train_ neuron rate data_set = if is_good data_set neuron
                         then neuron
                         else let new_neuron = foldr (better_neuron rate) neuron data_set
                              in train_ new_neuron rate data_set

--Returns true if all of the points in the data set are correctly categorized by the neuron
is_good :: (Num a, Ord a) => [([a], a)] -> Neuron a -> Bool
is_good data_set neuron =
    let  correctness_list = [(neuron_fire neuron xs) * val > 0 | (xs, val) <- data_set]
    in foldr (&&) True correctness_list 

--Returns a default neuron for the trainer
default_neuron :: (Neuron Double)
default_neuron = Neuron (1:[0, 0..])

--Return a neuron that linearly separates data  
train :: [DataPoint Double] -> Neuron Double
train xs = train_ default_neuron 0.5 xs

--Return 1 if True, -1 if False
renormalize :: Bool -> Double
renormalize True = 1
renormalize False = -1

categorize :: [DataPoint Double] -> [Double] -> Bool
categorize xs = neuron_fire (train xs)
