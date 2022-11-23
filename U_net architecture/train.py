from model import UNet
import numpy as np
import torch
import torch.optim as optim
import random
import matplotlib.pyplot as plt
import tqdm

def criterion(output,target):
  return torch.mean((output-target)**2)

ch_in = 1
ch_out = 1
size = 128

fcn = UNet(ch_in,ch_out,size).cuda()  

optimizer = optim.Adam(fcn.parameters(), 10**(-3))


nepochs = 40 #se faire un thé en attendant
nbatches = 100
batchsize = 6

train_losses = []


def generate_rectangle(size):
    x=random.randint(1, size[1]-60)
    y=random.randint(1, size[0]-60)
    h=random.randint(4, 50)
    w=random.randint(4, 50)
    a=torch.zeros((size[1],size[0]))
    for i in range(h):
        for j in range(w):
            a[i+x,j+y]=1
    return a,x,y,h,w

def gen(bs, size):
    target=torch.empty((bs,3,size[1],size[0]))
    a=torch.zeros((bs,1,size[1],size[0]))
    for i in range(bs):
        for k in range(3):
            mat,x,y,h,w=generate_rectangle(size)
            a[i,0,:,:]+=mat
            target[i,k,:,:]=mat
    return a,target





for epoch in range(nepochs):
    

    print("Epoch " + str(epoch))
    epoch_losses  = []
    for i in tqdm.tqdm(range(nbatches)):    # nbatch = datasetsize/batchsize
        #Load inputs
        

        input,target = gen(batchsize,(128,128)) 

        #print(target.shape)
        input = input.cuda()
        target = target.cuda()

        
        optimizer.zero_grad()        
        output = fcn(input)
        #print(output.shape)
        loss = criterion(output,target)
        loss.backward()
        
        optimizer.step()


        epoch_losses.append(loss.detach().cpu())

        del target, input, loss
        torch.cuda.empty_cache()     

    epoch_loss = np.mean(epoch_losses)
    torch.save(fcn,"unet_rectangles.mdl")
    train_losses.append(epoch_loss)    
    print('epoch loss : \n')
    print(epoch_loss)