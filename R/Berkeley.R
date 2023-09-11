UCBAdmissions

male.admit =  UCBAdmissions[1,1,]
male.rej =  UCBAdmissions[2,1,]
female.admit =  UCBAdmissions[1,2,]
female.rej =  UCBAdmissions[2,2,]

male.admit.rate = sum(male.admit)/sum(male.rej + male.admit)
female.admit.rate = sum(female.admit)/sum(female.rej + female.admit)

## Z is the variable corresponding to the department
z.prob = c( sum(UCBAdmissions[,,1]), sum(UCBAdmissions[,,2]), 
            sum(UCBAdmissions[,,3]), sum(UCBAdmissions[,,4]),
            sum(UCBAdmissions[,,5]), sum(UCBAdmissions[,,6]))
z.prob = z.prob/sum(z.prob)

## X is the variable for gender, X=0, female; X=1, male
## Y is the admission status

## Estimator of the causal effect
E.Y1 = sum( male.admit/(male.admit+male.rej) * z.prob)
E.Y0 = sum( female.admit/(female.admit+female.rej) * z.prob)

## ATT 
E.Y1.X1 = sum(male.admit)/sum(male.admit+male.rej)
prob.z.X1 = (male.admit+male.rej)/sum(male.admit+male.rej)
E.Y0.X1 = sum( (female.admit/(female.admit+female.rej))*prob.z.X1 )