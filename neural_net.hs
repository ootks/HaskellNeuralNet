module Main where 

import Perceptron

data NeuralNet = Input Int | Hidden (Neuron Double) [NeuralNet]

get_output :: NeuralNet -> [Double] -> Double
get_output (Input pos) inputs = inputs !! pos
get_output (Hidden neuron dependencies) inputs = neuron_fire neuron [get_output dependency inputs | dependency <- dependencies]

make_neural_net = 
    let input_1 = Input 0
        input_2 = Input 1
    in Hidden (train test_data) [input_1, input_2]

main = do 
    print "Using Test Data"
    print test_data
    print $ train test_data
    print $ get_output $ (make_neural_net) (fst (test_data !! 0))
