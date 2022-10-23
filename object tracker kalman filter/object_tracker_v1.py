import cv2 as cv
import numpy as np


u = (160//2, 80*255//100, 80*255//100)
l = (90//2, 30*255//100, 30*255//100)

def detect_obj(image,l,u):
    mask = cv.inRange(cv.cvtColor(image, cv.COLOR_BGR2HSV), l, u)
    
    mask = cv.erode(mask, None, iterations=1)
    mask = cv.dilate(mask, None, iterations=3)

    elements = cv.findContours(
        mask, cv.RETR_EXTERNAL, cv.CHAIN_APPROX_SIMPLE)[-2]

    #result = cv.bitwise_and(frame, frame, mask=mask)
    c=None
    point=None
    radius=0
    if elements:
        c = max(elements, key=cv.contourArea)
        ((x, y), radius) = cv.minEnclosingCircle(c)
        point=np.array([np.int32(x), np.int32(y)],dtype=np.int32)
    return point,np.int32(radius),mask,c

if __name__=="__main__":
    cap = cv.VideoCapture(0)
    while cap.isOpened():
        ret, frame = cap.read()

        # print(np.max(hsl_frame[:,:,0]))

        #mask = get_mask(frame, 60,5)
        point,r,_,c =detect_obj(frame,l,u)
        #result = cv.bitwise_and(frame, frame, mask=mask)
        if point is not None:
            
            cv.circle(frame, point,
                    r, (0, 255, 255))
            cv.circle(frame, point,
                    1, (255,0,0),10)
            cv.polylines(frame, [c], True, (0, 255, 0))

        cv.imshow('webcam', frame)

        #cv.imshow('webcam_filtred', result)
        key = cv.waitKey(10) & 0xff

        if key == ord('q'):
            break

    cv.destroyAllWindows()
    cap.release()

