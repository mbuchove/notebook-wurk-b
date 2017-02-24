#!/usr/bin/env python2
"""
employee ids
"""

class employee():
    """class to contain info about an employee"""
    def __init__(self, eid):
        self.eid = eid
        self.workers = [] 
        
    def summary(self):
        print self.eid, self.mid, self.name 
        
    def add_worker(self, worker):
        """adds worker under person to list"""
        self.workers.append(worker)
        
    def print_workers(self, head_string):
        """print out the workers below"""
        if head_string != "":
            print head_string[0:len(head_string)-2] + "|_" + self.name
        else:
            print self.name
        l = len(self.workers)
        for i in xrange(0, l):
            if i != l-1:
                self.workers[i].print_workers(head_string+"| ")
            else:
                self.workers[i].print_workers(head_string+"  ")
        #for worker in self.workers:
        #    worker.print_workers(indent_level+1)
        

# put into main 

employee_list = []
employee_list.append((1, 1, "Bob McBobby"))
employee_list.append((2, 1, "Emp2"))
employee_list.append((3, 2, "Emp3"))
employee_list.append((4, 3, "Emp4"))
employee_list.append((5, 4, "Emp5"))
employee_list.append((6, 3, "Emp6"))
employee_list.append((7, 1, "emp7"))



employee_dict = {}

for emp_info in employee_list:
    eid = emp_info[0]
    mid = emp_info[1]
    if mid not in employee_dict:
        employee_dict[mid] = employee(mid)
    if eid not in employee_dict:
        employee_dict[eid] = employee(eid)
    employee_dict[eid].mid = mid
    employee_dict[eid].name = emp_info[2]
    if eid != mid:
        employee_dict[mid].add_worker(employee_dict[eid]) 
    else:
        CEO = employee_dict[eid]
       
   
CEO.print_workers("")
    
