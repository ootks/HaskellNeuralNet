data Neuron a = Neuron ([a]) deriving Show

--Return the dot product of Neuron a with  
neuron_fire :: (Num a, Ord a) => Neuron a -> [a] -> a
neuron_fire (Neuron (w_zero:ws)) xs = (foldr (+) w_zero (zipWith (*)  ws xs))

better_neuron :: (Num a, Ord a) => a -> ([a], a) -> Neuron a -> Neuron a
better_neuron rate (xs, val) neuron@(Neuron weights)=
    let fire = neuron_fire neuron xs
        new_weights = zipWith (+) weights [rate * (val - fire) | x<-xs]
    in (Neuron new_weights)

train_neuron :: (Num a, Ord a) => a -> [([a], a)] -> Neuron a -> Neuron a
train_neuron rate data_set neuron = if is_good data_set neuron
                         then neuron
                         else let new_neuron = foldr (better_neuron rate) neuron data_set
                              in train_neuron rate data_set new_neuron


is_good :: (Num a, Ord a) => [([a], a)] -> Neuron a -> Bool
is_good data_set neuron =
    let  correctness_list = [(neuron_fire neuron xs) * val > 0 | (xs, val) <- data_set]
    in foldr (&&) True correctness_list 

default_neuron :: (Neuron Double)
default_neuron = Neuron [1, 0]
