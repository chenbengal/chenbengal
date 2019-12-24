# -*- coding: utf-8 -*-
"""
Created on Wed Nov 13 21:26:54 2019

@author: chen
"""

#imports
import numpy as np
import pandas as pd 
from keras.preprocessing.image import ImageDataGenerator, load_img
from keras.utils import to_categorical
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import random
import os

#Define constans
FAST_RUN = False
IMAGE_WIDTH=128
IMAGE_HEIGHT=128
IMAGE_SIZE=(IMAGE_WIDTH, IMAGE_HEIGHT)
IMAGE_CHANNELS=3

#get files
filesnames = os.listdir("C:\\Users\chen\\Desktop\\dogscats\\training_set\\train")
categories =  []
for i in filesnames:
    categories.append(i.split('.')[0])


df_train = pd.DataFrame({'filename' : filesnames, 'category': categories})
df_train.head(15)

len(df_train[df_train.category =='cat'])
len(df_train[df_train.category =='dog'])

#cats samples
sample = random.choice(df_train.filename)
image = load_img("C:\\Users\chen\\Desktop\\dogscats\\training_set\\train\\"+sample)
plt.imshow(image)



#CREATE MODEL
from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Dropout, Flatten, Dense, Activation, BatchNormalization
model = Sequential()
model.add(Conv2D(32,(3,3), activation = 'relu', input_shape = (IMAGE_WIDTH, IMAGE_HEIGHT, IMAGE_CHANNELS)))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Conv2D(64, (3, 3), activation='relu'))
model.add(BatchNormalization())
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Conv2D(128, (3, 3), activation='relu'))
model.add(BatchNormalization())
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Flatten())
model.add(Dense(512, activation='relu'))
model.add(BatchNormalization())
model.add(Dropout(0.5))
model.add(Dense(2, activation='softmax')) # 2 because we have cat and dog classes

model.compile(loss='categorical_crossentropy', optimizer='rmsprop', metrics=['accuracy'])

model.summary()

#EARLY STOP
from keras.callbacks import EarlyStopping, ReduceLROnPlateau
earlystop = EarlyStopping(patience=10)

#learning_rate
learning_rate_reduction = ReduceLROnPlateau(monitor='val_acc', 
                                            patience=2, 
                                            verbose=1, 
                                            factor=0.5, 
                                            min_lr=0.00001)


callbacks = [earlystop, learning_rate_reduction]

#SPLIT TRAIN VALIDATE
train_df, validate_df = train_test_split(df_train, test_size=0.20, random_state=42)
train_df = train_df.reset_index(drop=True)
validate_df = validate_df.reset_index(drop=True)

#Check sizes
train_df['category'].value_counts().plot.bar()
validate_df['category'].value_counts().plot.bar()

total_train = train_df.shape[0]
total_validate = validate_df.shape[0]
batch_size=15



#create generate dataset
train_datagen = ImageDataGenerator(
    rotation_range=15,
    rescale=1./255,
    shear_range=0.1,
    zoom_range=0.2,
    horizontal_flip=True,
    width_shift_range=0.1,
    height_shift_range=0.1
)
batch_size = 15

train_generator = train_datagen.flow_from_dataframe(
    train_df, 
    "C:\\Users\chen\\Desktop\\dogscats\\training_set\\train\\", 
    x_col='filename',
    y_col='category',
    target_size=IMAGE_SIZE,
    class_mode='categorical',
    batch_size=batch_size
)

validation_datagen = ImageDataGenerator(rescale=1./255)
validation_generator = validation_datagen.flow_from_dataframe(
    validate_df, 
    "C:\\Users\chen\\Desktop\\dogscats\\training_set\\train\\", 
    x_col='filename',
    y_col='category',
    target_size=IMAGE_SIZE,
    class_mode='categorical',
    batch_size=batch_size
)

#RUN MODEL
FAST_RUN = False
epochs=3 if FAST_RUN else 10
history = model.fit_generator(
    train_generator, 
    epochs=epochs,
    validation_data=validation_generator,
    validation_steps=total_validate//batch_size,
    steps_per_epoch=total_train//batch_size,
    callbacks=callbacks
)

#SAVE MODEL
model.save_weights("model.h5")

#VIRTUALIZE TRAINING
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 12))
ax1.plot(history.history['loss'], color='b', label="Training loss")
ax1.plot(history.history['val_loss'], color='r', label="validation loss")
ax1.set_xticks(np.arange(1, epochs, 1))
#ax1.set_yticks(np.arange(0, 1, 0.1))
legend = ax1.legend(loc='best', shadow=True)

ax2.plot(history.history['acc'], color='b', label="Training accuracy")
ax2.plot(history.history['val_acc'], color='r',label="Validation accuracy")
ax2.set_xticks(np.arange(1, epochs, 1))

legend = plt.legend(loc='best', shadow=True)
plt.tight_layout()
plt.show()

#TEST DATASET
test_filenames = os.listdir("C:\\Users\chen\\Desktop\\dogscats\\test_set\\test\\")
test_df = pd.DataFrame({
    'filename': test_filenames
})
nb_samples = test_df.shape[0]

test_gen = ImageDataGenerator(rescale=1./255)
test_generator = test_gen.flow_from_dataframe(
    test_df, 
    "C:\\Users\chen\\Desktop\\dogscats\\test_set\\test\\", 
    x_col='filename',
    y_col=None,
    class_mode=None,
    target_size=IMAGE_SIZE,
    batch_size=batch_size,
    shuffle=False)
    
predict = model.predict_generator(test_generator, steps=np.ceil(nb_samples/batch_size))
test_df['category'] = np.argmax(predict, axis=-1)
test_df['category'] = test_df['category'].replace({1:'dog', 0:'cat'})
test_df['category'].value_counts().plot.bar()

#SEE THE PREDICTIONS
sample_test = test_df.head(18)
sample_test.head()
plt.figure(figsize=(12, 24))
for index, row in sample_test.iterrows():
    filename = row['filename']
    category = row['category']
    img = load_img("C:\\Users\chen\\Desktop\\dogscats\\test_set\\test\\"+filename, target_size=IMAGE_SIZE)
    plt.subplot(6, 3, index+1)
    plt.imshow(img)
    plt.xlabel(filename + '(' + "{}".format(category) + ')' )
plt.tight_layout()
plt.show()

predict[:10]
test_df['category'][:10]

#see the photo of the dogs that the model was the sures about
spots = np.argsort(-predict[:,1])[:18]
sample_test = test_df.iloc[spots,:].reset_index(drop = True)
plt.figure(figsize=(12, 24))
for index, row in sample_test.iterrows():
    filename = row['filename']
    category = row['category']
    img = load_img("C:\\Users\chen\\Desktop\\dogscats\\test_set\\test\\"+filename, target_size=IMAGE_SIZE)
    plt.subplot(6, 3, index+1)
    plt.imshow(img)
    plt.xlabel(filename + '(' + "{}".format(category) + ')' )
plt.tight_layout()
plt.show()

#see the photo of the cat that the model was surest about
spots = np.argsort(predict[:,1])[:18]
sample_test = test_df.iloc[spots,:].reset_index(drop = True)
plt.figure(figsize=(12, 24))
for index, row in sample_test.iterrows():
    filename = row['filename']
    category = row['category']
    img = load_img("C:\\Users\chen\\Desktop\\dogscats\\test_set\\test\\"+filename, target_size=IMAGE_SIZE)
    plt.subplot(6, 3, index+1)
    plt.imshow(img)
    plt.xlabel(filename + '(' + "{}".format(category) + ')' )
plt.tight_layout()
plt.show()

#see the most confused photo
spots = np.argsort(abs(predict[:,1]-predict[:,0]))[:18]
sample_test = test_df.iloc[spots,:].reset_index(drop = True)
plt.figure(figsize=(12, 24))
for index, row in sample_test.iterrows():
    filename = row['filename']
    category = row['category']
    img = load_img("C:\\Users\chen\\Desktop\\dogscats\\test_set\\test\\"+filename, target_size=IMAGE_SIZE)
    plt.subplot(6, 3, index+1)
    plt.imshow(img)
    plt.xlabel(filename + '(' + "{}".format(category) + ')' )
plt.tight_layout()
plt.show()

#check if the spots showen are correct
predict[test_df.filename=='dog.4030.jpg',:]


#LOGISTIC REGRESSION
import cv2
train_from_im_to_df = []
for image in train_df.filename:
    path = os.path.join("C:\\Users\chen\\Desktop\\dogscats\\training_set\\train\\",image)
    image = cv2.imread(path,cv2.IMREAD_GRAYSCALE)
    image = cv2.resize(image, (64,64))
    train_from_im_to_df.append(image)

#train_from_im_to_df
train_categories = train_df.category
train_from_im_to_df = np.array(train_from_im_to_df)
#train_from_im_to_df.shape
train_from_im_to_df = (train_from_im_to_df-np.min(train_from_im_to_df))/(np.max(train_from_im_to_df)-np.min(train_from_im_to_df))
#train_from_im_to_df[1,1]
#train_from_im_to_df.train

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
grid={"C":np.logspace(-3,3,7),"penalty":["l1","l2"]},
logistic_regression=LogisticRegression(random_state=42)
log_reg_cv=GridSearchCV(logistic_regression,grid,cv=10)
train_flatten = train_from_im_to_df.reshape(len(train_from_im_to_df),train_from_im_to_df.shape[1]*train_from_im_to_df.shape[2])
train_flatten.shape

result_lr = logistic_regression.fit(train_flatten,train_categories) 
#print(result_lr)

y_pred = logistic_regression.predict(train_flatten)
print('Accuracy of logistic regression classifier on test set: {:.2f}'.format(logistic_regression.score(train_flatten, train_categories)))

#prepare the validate samples
validate_from_im_to_df = []
for image in validate_df.filename:
    path = os.path.join("C:\\Users\chen\\Desktop\\dogscats\\training_set\\train\\",image)
    image = cv2.imread(path,cv2.IMREAD_GRAYSCALE)
    
    image = cv2.resize(image, (64,64))
    validate_from_im_to_df.append(image)

#train_from_im_to_df
validate_categories = validate_df.category
validate_from_im_to_df = np.array(validate_from_im_to_df)
#train_from_im_to_df.shape
validate_from_im_to_df = (validate_from_im_to_df-np.min(validate_from_im_to_df))/(np.max(validate_from_im_to_df)-np.min(validate_from_im_to_df))
#train_from_im_to_df[1,1]
validate_flatten = validate_from_im_to_df.reshape(len(validate_from_im_to_df),validate_from_im_to_df.shape[1]*validate_from_im_to_df.shape[2])
validate_flatten.shape
y_pred_val = logistic_regression.predict(validate_flatten)
print('Accuracy of logistic regression classifier on val set: {:.2f}'.format(logistic_regression.score(validate_flatten, validate_categories)))


from sklearn.metrics import confusion_matrix
confusion_matrix = confusion_matrix(validate_categories, y_pred_val)
print(confusion_matrix)


#see the data
sample_test = validate_df.head(18)
sample_test.head()
plt.figure(figsize=(12, 24))
for index, row in sample_test.iterrows():
    filename = row['filename']
    category = row['category']
    img = cv2.imread("C:\\Users\chen\\Desktop\\dogscats\\training_set\\train\\"+filename, cv2.IMREAD_GRAYSCALE)
    img = cv2.resize(img,(64,64))
    plt.subplot(6, 3, index+1)
    plt.imshow(img)
    plt.xlabel(filename + '(' + "{}".format(category) + ')' )
plt.tight_layout()
plt.show()

