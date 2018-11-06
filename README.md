# Project: Can you unscramble a blurry image? 
![image](figs/example.png)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2018

+ Team Group 8
+ Team members

	+ Benedict, Gabriel gb2661@columbia.edu
	+ Qiaqia Sun qs2184@columbia.edu
	+ Zhangyang Xu zx2229@columbia.edu
	+ Huiyu Zhang hz2497@columbia.edu 

+ Project summary: In this project, we created a classification engine for enhance the resolution of images. Genarally, We have achieved three things in this project: 1.Implemented the current practice as the baseline model. 2.Implemented an improvement to the current practice.In this part, We chose three different models as improving models: Xgboost, Neural Network and Random Forest. 3.Evaluated the performance gain of our proposed improvement against the baseline. 
    
+ Specifically, in feature selection part, we implemented Lagrange transformation to training pictures, in order to find the most     representive points in each picture and sampled them as our training data points. 
Then after building models using training data, through cross validation for each model (metioned above), we got the best parameters for each model according to the cross validation error, then implemented each model with their best parameters on testing pictures.
Finally, according to the PSNR value of each model, we found the best model in super resolution problem is    .
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
