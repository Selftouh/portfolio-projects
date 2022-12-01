##############################################################################################
################################   UNet (parties)#############################################
import torch
import torch.nn as nn
import torch.nn.functional as F

class double_conv(nn.Module):
    '''(conv => BN => ReLU) * 2'''
    def __init__(self, in_ch, out_ch):
        super(double_conv, self).__init__()
        self.conv = nn.Sequential(

            nn.Conv2d(in_ch, out_ch, 3, padding=1),
            nn.BatchNorm2d(out_ch),
            nn.ReLU(inplace=True),

            nn.Conv2d(out_ch, out_ch, 3, padding=1),
            nn.BatchNorm2d(out_ch),
            nn.ReLU(inplace=True)

        )

    def forward(self, x):
        x = self.conv(x)
        return x


class inconv(nn.Module):
    def __init__(self, in_ch, out_ch):
        super(inconv, self).__init__()
        self.conv = double_conv(in_ch, out_ch)

    def forward(self, x):
        x = self.conv(x)
        return x


class Down(nn.Module):
    def __init__(self, in_ch, out_ch):
        super(Down, self).__init__()
        self.mpconv = nn.Sequential(
            nn.MaxPool2d(2),
            double_conv(in_ch, out_ch)
        )

    def forward(self, x):
        x = self.mpconv(x)
        return x

#Given transposed=1, weight[1024, 256, 2, 2], so expected input[64, 512, 4, 4] to have 1024 channels, but got 512 channels instead
        
    
    
class Up(nn.Module):
    def __init__(self, in_ch, out_ch, bilinear=False):
        super(Up, self).__init__()
        if bilinear:
            self.up = nn.Upsample(scale_factor=2, mode='bilinear')#nn.UpsamplingBilinear2d(scale_factor=2)
        else:
            self.up = nn.ConvTranspose2d(in_ch, in_ch, kernel_size=2, stride=2)

        self.conv = double_conv(2*in_ch, out_ch)

    def forward(self, x1, x2):
        x1 = self.up(x1)
        diffX = x1.size()[2] - x2.size()[2]
        diffY = x1.size()[3] - x2.size()[3]
        x2 = F.pad(x2, (diffX // 2, int(diffX / 2),
                        diffY // 2, int(diffY / 2)))
        x = torch.cat([x2, x1], dim=1)
        x = self.conv(x)
        return x


class outconv(nn.Module):
    def __init__(self, in_ch, out_ch):
        super(outconv, self).__init__()
        self.conv = nn.Conv2d(in_ch, out_ch, 1)
        

    def forward(self, x):
        x = self.conv(x)
        return x

class outdense(nn.Module):
    def __init__(self,  out_vector):
        super(outdense, self).__init__()
        self.dense1 = nn.LazyLinear(50)
        self.dense2 = nn.LazyLinear(20)
        self.dense3 = nn.LazyLinear(out_vector)
        

    def forward(self, x):
        x = x.view(1,-1)
        x = self.dense1(x)
        x= nn.ReLU()(x)
        x = self.dense2(x)
        x= nn.ReLU()(x)
        x = self.dense3(x)
        x= nn.ReLU()(x)
        return x


###############################################################################################################################################
########################################Montage Unet ################################################################################

class UNet(nn.Module):
    def __init__(self, n_channels, n_classes,size=64):
        super(UNet, self).__init__()
        self.inc = inconv(n_channels, size)
        self.down1 = Down(size, 2*size)
        self.down2 = Down(2*size, 4*size)
        self.down3 = Down(4*size, 8*size)
        self.down4 = Down(8*size, 8*size)
        self.up1 = Up(8*size, 4*size)
        #self.up12 = up(16*size, 4*size)
        self.up2 = Up(4*size, 2*size)
        #self.up22 = up(8*size, 2*size)
        self.up3 = Up(2*size, size)
        #self.up32 = up(4*size, size)
        self.up4 = Up(size, size)
        #self.up42 = up(2*size, size)
        self.outc = outconv(size, n_classes)
        self.outc2 = outconv(size, n_classes)
        self.n_classes=n_classes
        
    def forward(self, x):
        x1 = self.inc(x)
        x2 = self.down1(x1)
        x3 = self.down2(x2)
        x4 = self.down3(x3)
        x5 = self.down4(x4)
        x = self.up1(x5, x4)
        del x4, x5
        x = self.up2(x, x3)
        del x3
        x = self.up3(x, x2)
        del x2
        x = self.up4(x, x1)
        del x1
        x = self.outc(x) 
        return   x


class WNet(nn.Module):
    def __init__(self, n_channels, n_classes,size=64):
        super(WNet, self).__init__()
        self.inc = inconv(n_channels, size)
        self.down1 = Down(size, 2*size)
        self.down2 = Down(2*size, 4*size)
        self.down3 = Down(4*size, 8*size)
        self.down4 = Down(8*size, 8*size)
        self.up1 = Up(8*size, 4*size)
        self.up12 = Up(8*size, 4*size)
        self.up2 = Up(4*size, 2*size)
        self.up22 = Up(4*size, 2*size)
        self.up3 = Up(2*size, size)
        self.up32 = Up(2*size, size)
        self.up4 = Up(size, size)
        self.up42 = Up(size, size)
        self.outc = outconv(size, n_classes)
        self.outl = outdense(1)
        self.n_classes=n_classes
        
    def forward(self, x):
        x1 = self.inc(x)
        x2 = self.down1(x1)
        x3 = self.down2(x2)
        x4 = self.down3(x3)
        x5 = self.down4(x4)
        x = self.up1(x5, x4)
        x_2 = self.up12(x5, x4)
        del x4, x5
        x = self.up2(x, x3)
        x_2 = self.up22(x_2, x3)
        del x3
        x = self.up3(x, x2)
        x_2 = self.up32(x_2, x2)
        del x2
        x = self.up4(x, x1)
        x_2 = self.up42(x_2, x1)
        del x1
        x = self.outc(x) 
        
        x_2= self.outl(x_2)
        return   x,x_2


if __name__=="__main__":
    model=WNet(3,1)
    a=torch.ones((1,3,50,50))*0.21
    y,y2=model(a)
    print(y,y2)