from model import UNet
import numpy as np
import torch as t
import torch.optim as optim
import random
import matplotlib.pyplot as plt
import tqdm


def train(model, criterion, accuracy, dataloader, nepochs, device):
    optimizer = optim.Adam(model.parameters(), 10**(-3))
    train_losses = []
    train_accs = []
    for epoch in range(nepochs):

        print("Epoch " + str(epoch))
        epoch_losses = []
        epoch_accuracies = []
        # nbatch = datasetsize/batchsize
        for input, target, label in tqdm.tqdm(dataloader):
            # Load inputs
            # input=input.to(device)
            # target=target.to(device)

            optimizer.zero_grad()
            output = model(input)
            # print(output.shape)
            loss = criterion(output, target)
            loss.backward()

            optimizer.step()

            epoch_accuracies.append(accuracy(output, target))
            epoch_losses.append(loss.detach().cpu())

            del target, input, loss
            t.cuda.empty_cache()

        epoch_loss = np.mean(epoch_losses)
        epoch_accuracy = np.mean(epoch_accuracies)
        t.save(model, "unet_crowd_counting.mdl")
        train_losses.append(epoch_loss)
        train_accs.append(epoch_accuracy)
        print('epoch loss : ', epoch_loss)
        print('epoch acc : ', epoch_accuracy)


def train_Wnet(model, criterion1, criterion2, accuracy1, accuracy2, dataloader, nepochs, device,lr=10**(-2)):
    optimizer = optim.Adam(model.parameters(), lr)
    train_losses = []
    train_accs = []
    train_losses2 = []
    train_accs2 = []
    for epoch in range(nepochs):

        print("Epoch " + str(epoch))
        epoch_losses1 = []
        epoch_accuracies = []
        epoch_losses2 = []
        epoch_accuracies2 = []
        # nbatch = datasetsize/batchsize
        for input, target, label in tqdm.tqdm(dataloader):
            # Load inputs
            # input=input.to(device)
            # target=target.to(device)

            optimizer.zero_grad()

            output1, output2 = model(input)
            # print(output.shape)
            loss1 = criterion1(output1, target)
            loss2 = criterion2(output2, label)
            loss = loss1+loss2
            loss.backward()

            optimizer.step()

            acc1 = accuracy1(output1, target)
            acc2 = accuracy2(output2, label)

            epoch_accuracies.append(acc1.detach().cpu())
            epoch_losses1.append(loss1.detach().cpu())
            #epoch_accuracies2.append(acc2.detach().cpu())
            #epoch_losses2.append(loss2.detach().cpu())

            del target, input, label, loss1, loss2, acc1, acc2
            t.cuda.empty_cache()

        
        t.save(model, "wnet_crowd_counting.mdl")
        epoch_loss = np.mean(epoch_losses1)
        epoch_accuracy = np.mean(epoch_accuracies)
        #epoch_loss2 = np.mean(epoch_losses2)
        #epoch_accuracy2 = np.mean(epoch_accuracies2)

        train_losses.append(epoch_loss)
        train_accs.append(epoch_accuracy)
        #train_losses2.append(epoch_loss2)
        t#rain_accs2.append(epoch_accuracy2)
        print('epoch loss [seg,counting] : ', epoch_loss)
        print('epoch acc [seg,counting] : ', epoch_accuracy)
