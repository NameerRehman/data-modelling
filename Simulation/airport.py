# -*- coding: utf-8 -*-
"""
Created on Tue Mar 15 21:11:18 2022

@author: namee

---------------------------
Question 13.2

Running the simulation below for lambda=5, the optimal number for id/boarding pass checkers is 4 and 
personal check queues is also 4 to keep wait time below 15min. For lambda=50, this number changes to 
around 36 and 38 respectively 
"""

import random
import simpy

#need to keep wait times below 15 minutes 
#find optimal number of id and personal checkers (scanners) to achieve this 
boarding_checkers = 4 
personal_checkers = 4

#36 and 38

arrival_rate = 5 #lamba - poisson distribution
mean_idcheck_time = 0.75 # service time for ID/boarding check queue
min_scantime, max_scantime = 0.5, 1.0 #min and max scan time for peronal check queue
reps = 100 # number of reps

#initiate environment & its variables/distributions
class Airport(object):
    def __init__(self,env):
        self.env = env
        
        #identify number of ID checkers 
        self.checker = simpy.Resource(env,boarding_checkers) 
        
        #identify number of personal checkers and set queue for each 
        self.scanner = [] 
        for i in range(personal_checkers):
            self.scanner.append(simpy.Resource(env,1))

    #exponential service rate which is 1/expovariate
    def boarding(self,passenger):
        yield self.env.timeout(random.expovariate(1/mean_idcheck_time))

    # uniform scan time ranging from min to max (0.5-1.0)
    def scan(self,passenger):
        yield self.env.timeout(random.uniform(min_scantime,max_scantime))

# Passenger arrival process
def setup(env):
    i = 0
    airport = Airport(env)
    while True: 
        yield env.timeout(random.expovariate(arrival_rate))
        i += 1 # count one more passenger

        # send the passenger through its process
        env.process(passenger(env,'Passenger %d' % i,airport)) 
        

# simulate passenger processing times 
def passenger(env,name,airport):
    arrival_time = env.now 
   
    # id/boarding pass check 
    with airport.checker.request() as request:
        yield request
        in_time = env.now
        yield env.process(airport.boarding(name)) 
        out_time = env.now 
        idcheck_time.append(out_time - in_time) 

    #personal scan - multiple lines
    shortest_queue = 0
    for i in range(1,personal_checkers):
        if (len(airport.scanner[i].queue) < len(airport.scanner[shortest_queue].queue)):
            shortest_queue = i

    # Go through scanner queue
    with airport.scanner[shortest_queue].request() as request: 
          yield request 
          in_time = env.now 
          yield env.process(airport.scan(name))
          out_time = env.now 
          scan_time.append(out_time - in_time) 
      
    global time_spent
    global people_counter        
    timeLeave = env.now 
    time_spent.append(timeLeave - arrival_time)
    people_counter += 1 



#Run simulation
avg_id_time, avg_scan_time, avg_airport_time, avg_wait_time = [],[],[],[] 

# for each replication
for i in range(reps):

    random.seed(i)

    people_counter = 0
    idcheck_time = []
    scan_time = []
    time_spent = []
    
    # create environment & run
    env = simpy.Environment()
    env.process(setup(env)) # start passenger arrival process
    env.run(until=60*12) # run simulation for 12 hours 
 
    avg_airport_time.append(sum(time_spent[1:people_counter]) / people_counter)
    avg_id_time.append(sum(idcheck_time[1:people_counter]) / people_counter)
    avg_scan_time.append(sum(scan_time[1:people_counter]) / people_counter)
    avg_wait_time.append(avg_airport_time[i] - avg_id_time[i] - avg_scan_time[i])

    print(f'# {i} - Average wait: {avg_wait_time[i]} minutes')


print(f'average wait time accross all simulations: {sum(avg_wait_time)/reps}')
