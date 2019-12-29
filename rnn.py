"""
Minimal character-level Vanilla RNN model. Written by Andrej Karpathy (@karpathy)
Revised by Evan Kirsch (@etkirsch)
BSD License
"""
import numpy as np
import json, os.path, base64, copy, random


def encode(ndarray):
    return json.dumps([str(ndarray.dtype),base64.b64encode(ndarray).decode("utf-8"),ndarray.shape])

def decode(jsonDump):
    loaded = json.loads(jsonDump)
    dtype = np.dtype(loaded[0])
    arr = np.frombuffer(base64.decodestring(loaded[1]),dtype)
    if len(loaded) > 2:
        return arr.reshape(loaded[2])
    return arr

class Rnn(object):
    def __init__(self, filename):
        # hyperparameters
        self.hidden_size = 25 # size of hidden layer of neurons
        self.seq_length = 25 # number of steps to unroll the RNN for
        self.learning_rate = 1e-1
        self.iterations_per_log = 100000

        self.readFile(filename)
        self.getWeights()

    def readFile(self, filename):
        '''Get data from a specified filename for use in this RNN'''
        self.data_name = filename + '.rnnpy'
        with open(filename, 'r') as file_data:
            self.data = file_data.read() # should be simple plain text file
        self.chars = list(set(self.data))
        self.data_size, self.vocab_size = len(self.data), len(self.chars)
        print('data has {0} characters, {1} of which are unique.'.format(self.data_size, self.vocab_size))
        self.char_to_ix = { ch:i for i,ch in enumerate(self.chars) }
        self.ix_to_char = { i:ch for i,ch in enumerate(self.chars) }

    def getWeights(self):
        '''
        Loads weights and biases from '(filename).rnnpy' if it exists; randomizes them if not
        - Evan Kirsch
        '''
        # randomize the model params
        self.Wxh = np.random.randn(self.hidden_size, self.vocab_size)*0.01 # input to hidden
        self.Whh = np.random.randn(self.hidden_size, self.hidden_size)*0.01 # hidden to hidden
        self.Why = np.random.randn(self.vocab_size, self.hidden_size)*0.01 # hidden to output
        self.bh = np.zeros((self.hidden_size, 1)) # hidden bias
        self.by = np.zeros((self.vocab_size, 1)) # output bias
        self.hprev = np.zeros((self.hidden_size,1))

        # memory variables for Adagrad
        self.mWxh = np.zeros_like(self.Wxh)
        self.mWhh = np.zeros_like(self.Whh)
        self.mWhy = np.zeros_like(self.Why)
        self.mbh = np.zeros_like(self.bh)
        self.mby = np.zeros_like(self.by)

        if os.path.isfile(self.data_name):
            try:
                saved_parameters = json.load(open(self.data_name,'r'))
                self.Wxh = copy.copy(decode(saved_parameters['Wxh']))
                self.Whh = copy.copy(decode(saved_parameters['Whh']))
                self.Why = copy.copy(decode(saved_parameters['Why']))
                self.bh = copy.copy(decode(saved_parameters['bh']))
                self.by = copy.copy(decode(saved_parameters['by']))
                self.hprev = copy.copy(decode(saved_parameters['hprev']))
                self.mWxh = copy.copy(decode(saved_parameters['mWxh']))
                self.mWhh = copy.copy(decode(saved_parameters['mWhh']))
                self.mWhy = copy.copy(decode(saved_parameters['mWhy']))
                self.mbh = copy.copy(decode(saved_parameters['mbh']))
                self.mby = copy.copy(decode(saved_parameters['mby']))
                print('Loaded previous results')
                return
            except Exception as msg:
                print(msg)


    def saveParameters(self):
        '''Saves all weights and biases into a file for continuity'''
        data = {}
        data['Wxh'] = encode(self.Wxh)
        data['Whh'] = encode(self.Whh)
        data['Why'] = encode(self.Why)
        data['bh'] = encode(self.bh)
        data['by'] = encode(self.by)
        data['hprev'] = encode(self.hprev)
        data['mWxh'] = encode(self.mWxh)
        data['mWhh'] = encode(self.mWhh)
        data['mWhy'] = encode(self.mWhy)
        data['mbh'] = encode(self.mbh)
        data['mby'] = encode(self.mby)
        with open(self.data_name,'w') as outfile:
            json.dump(data, outfile)

    def lossFun(self, inputs, targets):
        """
        inputs,targets are both list of integers.
        hprev is Hx1 array of initial hidden state
        returns the loss, gradients on model parameters, and last hidden state
        """
        step = {"x": {}, "h": {}, "y": {}, "p": {}}
        step['h'][-1] = np.copy(self.hprev)
        loss = 0
        # forward pass
        for t in np.arange(len(inputs)):
            step['x'][t] = np.zeros((self.vocab_size,1)) # encode in 1-of-k representation
            step['x'][t][inputs[t]] = 1
            step['h'][t] = np.tanh(np.dot(self.Wxh, step['x'][t]) + np.dot(self.Whh, step['h'][t-1]) + self.bh) # hidden state
            step['y'][t] = np.dot(self.Why, step['h'][t]) + self.by # unnormalized log probabilities for next chars
            step['y'][t] = np.exp(step['y'][t]) / np.sum(np.exp(step['y'][t])) # probabilities for next chars
            loss += -np.log(step['y'][t][targets[t],0]) # softmax (cross-entropy loss)
        # backward pass: compute gradients going backwards
        dWxh, dWhh, dWhy = np.zeros_like(self.Wxh), np.zeros_like(self.Whh), np.zeros_like(self.Why)
        dbh, dby = np.zeros_like(self.bh), np.zeros_like(self.by)
        dhnext = np.zeros_like(step['h'][0])
        for t in reversed(np.arange(len(inputs))):
            dy = np.copy(step['y'][t])
            dy[targets[t]] -= 1 # backprop into y
            dWhy += np.dot(dy, step['h'][t].T)
            dby += dy
            dh = np.dot(self.Why.T, dy) + dhnext # backprop into h
            dhraw = (1 - step['h'][t] * step['h'][t]) * dh # backprop through tanh nonlinearity
            dbh += dhraw
            dWxh += np.dot(dhraw, step['x'][t].T)
            dWhh += np.dot(dhraw, step['h'][t-1].T)
            dhnext = np.dot(self.Whh.T, dhraw)
        for dparam in [dWxh, dWhh, dWhy, dbh, dby]:
            np.clip(dparam, -5, 5, out=dparam) # clip to mitigate exploding gradients
        self.hprev = step['h'][len(inputs)-1]
        return loss, dWxh, dWhh, dWhy, dbh, dby

    def sample(self, seed_ix, n, training=True):
        """
        sample a sequence of integers from the model
        h is memory state, seed_ix is seed letter for first time step
        """
        x = np.zeros((self.vocab_size, 1))
        x[seed_ix] = 1
        ixes = []
        h = self.hprev
        for t in np.arange(n):
            h = np.tanh(np.dot(self.Wxh, x) + np.dot(self.Whh, h) + self.bh)
            y = np.dot(self.Why, h) + self.by
            p = np.exp(y) / np.sum(np.exp(y))
            ix = np.random.choice(range(self.vocab_size), p=p.ravel())
            x = np.zeros((self.vocab_size, 1))
            x[ix] = 1
            ixes.append(ix)
            if (self.ix_to_char[ix].isspace() and not training):
                return ixes
        return ixes

    def train(self, num_iterations=10000):
        '''
        Trains the neural net
        '''
        n, p = 0, 0
        while n < num_iterations:
            p = self.step(p)
            # Might want to deprecate this
            if n % self.iterations_per_log == 0:
                self.write(percentage=(float(n)/float(num_iterations)*100.0))
            n += 1 # iteration counter
        print('Training completed.')
        self.saveParameters()

    def write(self,percentage):
        '''
        Writes data to the commandline
        '''
        txt = self.randomSample(200)
        print('----\n {0} \n----\n{1}% of process completed'.format(txt,percentage))

    def randomSample(self,length):
        sample_ix = self.sample(random.randint(0,self.vocab_size-1), length)
        txt = ''.join(self.ix_to_char[ix] for ix in sample_ix)
        formatted = '\n'.join(txt.split())
        return formatted

    def step(self,p):
        '''
        Does the heavy lifting
        '''
        smooth_loss = -np.log(1.0/self.vocab_size)*self.seq_length # loss at iteration 0
        # prepare inputs (we're sweeping from left to right in steps seq_length long)
        if p+self.seq_length+1 >= len(self.data):
            self.hprev = np.zeros((self.hidden_size,1)) # reset RNN memory
            p = 0 # go from start of data

        inputs = [self.char_to_ix[ch] for ch in self.data[p:p+self.seq_length]]
        targets = [self.char_to_ix[ch] for ch in self.data[p+1:p+self.seq_length+1]]

        # forward seq_length characters through the net and fetch gradient
        loss, dWxh, dWhh, dWhy, dbh, dby = self.lossFun(inputs, targets)
        smooth_loss = smooth_loss * 0.999 + loss * 0.001

        # perform parameter update with Adagrad
        for param, dparam, mem in zip([self.Wxh, self.Whh, self.Why, self.bh, self.by],
                                [dWxh, dWhh, dWhy, dbh, dby],
                                [self.mWxh, self.mWhh, self.mWhy, self.mbh, self.mby]):
            mem += dparam * dparam
            param += -self.learning_rate * dparam / np.sqrt(mem + 1e-8) # adagrad update

        return p + self.seq_length # move data pointer
