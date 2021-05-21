payer=zeros(10000,5); 
% 1 - income 2- s 3-group 4-individual percieved audit rate audit rate,5-behaviour(0-truthfull, 1 strategic, 2 defiant)
collector = zeros(50,1);
payoff_p=zeros(10000,15);
payoff_c=zeros(50,15);
% normally distributing income
payer(1:4000,1)=normrnd(700000,200000,[4000,1]);
payer(4001:7000,1)=normrnd(3000000,500000,[3000,1]);
payer(7001:10000,1)=normrnd(10000000,3000000,[3000,1]);
% alotting groups to taxpayers
for i =1:10000
    payer(i,3)= mod(i,5)+1;
end

% a factor which increases the prob of audit of a payer
for i=1:10000
    if payer(i,1)> 3000000
        payer(i,4)= 1.5;
    elseif payer(i,1)< 500000
        payer(i,4)= .5;
    end    
end    
% putting s 
for i=1:10000
    t = rand;
    if t < .6
        payer(i,2)= rand;
        payer(i,5)= 1;
    elseif t < .8
            payer(i,2)=0;
            payer(i,5)= 2;
    else 
        payer(i,2)= 1;
        payer(i,5)=0;
    end 
end
t1=0;
       t2=0;
       t3=0;
      for i=1:10000
           if payer(i,2)< .1
               t3=t3+1;
           elseif payer(i,5) >.9
               t1=t1+1;
           else
               t2=t2+1;
           end
      end
      k=[t1 t2 t3];
dist=zeros(11,2);
for i=1:10000
    dist(floor(payer(i,2)*10+1),1)=dist(floor(payer(i,2)*10+1),1)+1;
end  
figure
bar(dist(:,1))
title('initial frequency distribution based on % of income reported')
set(gca,'xticklabel',[0:0.1:1])
% categorizing tax collectors(0-honest,1-corrupt)
for i=1:50
    t=rand;
    if t < .7;
        collector(i)= 0;
    else
        collector(i)= 1;
    end
end


r = 0.3; %tax rate
a = zeros(50,1); %audit rate
f = 2; %fine
for i=1:50
    a(i)= .05 + rand/10;
end    
S=zeros(100,3);
for itr=1:100
    audit= zeros(10000,1);
    for i=1:10000
        t=rand;
        if t< a(ceil(i/200))*payer(i,4);
            audit(i)=1;
        end
        c= ceil(i/200);
        if payer(i,5)==0;
            payoff_p(i,itr)= 0;
        elseif collector(c,1)==0
            payoff_p(i,itr)= r*(1-payer(i,2))*payer(i,1) - audit(i)* r*3*(1-payer(i,2))*payer(i,1);
            
        else
            payoff_p(i,itr)= r*(1-payer(i,2))*payer(i,1)- audit(i)*r*(1-payer(i,2))*payer(i,1)*(.8);
        end
   % updating values for next generation     
        score = zeros(50,1);
   %     
        if payoff_p(i,itr) < 0
            score(ceil(i/200),1)= score(ceil(i/200),1)+ 1;
            payer(i,4) = payer(i,4)*(2);
        elseif payoff_p(i,itr)== r*(1-payer(i,2))*payer(i,1)- r*(1-payer(i,2))*payer(i,1)*(.8);
            score(ceil(i/200),1)= score(ceil(i/200),1)+ 2;
            payer(i,4) = payer(i,4)*(1.5);
        else    
            payer(i,4) = payer(i,4)/1.2;
        end
    end     
   % neighbourhood effect
    for i=1:5:10000
        t=0;
        for j=1:5
            if payoff_p(i+j-1,itr)< 0 || payoff_p(i+j-1,itr)== .2*r*(1-payer(i+j-1,2))*payer(i+j-1,1) ;
                t=t+1;
            end
        end   
        for j=1:5
            if t>0
                payer(i+j-1,4)= payer(i+j-1,4)*t*1.5;
            else   
                payer(i+j-1,4)= payer(i+j-1,4)*.8;
            end
        end
    end
   for i=1:10000
       if payer(i,4)*a(ceil(i/200)) > .33 ;
           payer(i,2)=1;
           payer(i,5)=0;
       elseif  payoff_p(i,itr)<0 && payer(i,5)==1
           payer(i,2)= payer(i,2)+(1-payer(i,2))*(.5)*(rand);
       elseif  payoff_p(i,itr)==.2*r*(1-payer(i,2))*payer(i,1) && payer(i,5)>1
           payer(i,5)= payer(i,5)+1;
           if payer(i,5)>3;
               payer(i,2)=1;
               payer(i,5)=0;
           end
       elseif payoff_p(i,itr)>= 0;
           c= .02 + payoff_p(i,itr)*.2/payer(i,1);
           if payer(i,2)- c >=0
               payer(i,2)=payer(i,2)-c;
           end    
       end
   end
       t1=0;
       t2=0;
       t3=0;
      for i=1:10000
           if payer(i,2)< .1
               t3=t3+1;
           elseif payer(i,5) >.9
               t1=t1+1;
           else
               t2=t2+1;
           end
      end
   S(itr,1)=t1;
   S(itr,2)=t2;
   S(itr,3)=t3;
   for i=1:50
       if collector(i)==0 && score(i)>0
           a(i)=a(i)*(1.2^score(i));
       elseif collector(i)==1 && score(i)>0
           a(i)=a(i)*(1.2^(score(i)/2));
       else
           a(i)=a(i)*(.85);
       end  
   end
end   
for i=1:10000
    dist(floor(payer(i,2)*10+1),2)=dist(floor(payer(i,2)*10+1),2)+1;
end 
figure
bar(dist(:,2))
title('frequency distribution after 75 rounds based on % of income reported')
set(gca,'xticklabel',[0:0.1:1])
 S = [k;S];   
figure
   x=1:101;
   plot(x,S(:,1),'g',x,S(:,2),'b',x,S(:,3),'y')
   legend('Strategic','Compliant','Defiant')
   
   
           
   
        
    
    
    
    
    
    



    
    


