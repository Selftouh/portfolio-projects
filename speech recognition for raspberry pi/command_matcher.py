import numpy as np
from fuzzywuzzy import fuzz

OBJECTS = "light window door".split()
ATTRIBUTES = "red blue green bedroom livingroom kitchen".split()
ACTIONS = "turn off on close open raise start stop blink".split()


class connected_object(object):
    def __init__(self, label, actions=None, attributes=None) -> None:
        self.label = label
        self.actions = actions if actions else {}
        self.attributes = attributes if attributes else []

    def set_attribute(self, attribute):
        self.attributes+=[attribute]

    def set_action(self,label_action,action,*params):
        def f():
            action(*params)
        self.actions[label_action]=f

    def trigger(self,label_action):
        try:
            self.actions[label_action]()
            return True
        except(Exception):
            return False


class objects_network(object):
    def __init__(self,obj_classes,attribute_classes,actions_classes) -> None:
        self.obj_classes=obj_classes
        self.attribute_classes=attribute_classes
        self.actions_classes=actions_classes
        self.objects=[]

    def add_obj(self,obj:connected_object):
        self.objects+=[{'label':obj.label,'obj':obj,'actions':obj.actions,'attributes':obj.attributes}]

    def filter_by_attribute(self,attribute,list_obj=None):
        list_obj_=list_obj if list_obj else self.objects
        matching_matrix = np.zeros(len(list_obj_))
        for i,o in enumerate(list_obj_):
            if attribute in o['attributes']:
                matching_matrix[i]+=1
        return list_obj_[np.argmax(matching_matrix)]

    def trigger_object(self,label_obj,attribute,action):
        match_ratios=[match_word(w,label_obj) for w in self.obj_classes]
        i_max=np.argmax(match_ratios)
        the_obj_classe=self.obj_classes[i_max]
        the_obj=None
        list_obj=[]
        for ele in self.objects:
            if ele['label']==the_obj_classe:
                list_obj+=[ele]
        

        
        if len(list_obj)>1:
            the_obj=self.filter_by_attribute(attribute,list_obj)
        elif len(list_obj)==1:
            the_obj=list_obj[0]
        else:
            return False
            
        if the_obj:
            action_keys_list=list(the_obj['actions'].keys())
            actions_match_ratios=[match_word(w,action) for w in action_keys_list]
            the_action_label=action_keys_list[np.argmax(actions_match_ratios)]
            return the_obj['obj'].trigger(the_action_label)
        return False

def match_word(w1: str, w2: str):
    #return 1.0 if w1.lower() == w2.lower() else 0.0
    return fuzz.ratio(w1,w2)/100

def match_semantic(words,sentence):
    sentence_list = np.array(sentence.split())
    matching_matrix = np.zeros((len(sentence_list),len(words)))
    for i,w1 in enumerate(sentence_list):
        for j, w2 in enumerate(words):
            matching_matrix[i,j] = match_word(w1, w2)
    max=np.amax(matching_matrix)
    if max>0.0:
        index_max=np.argwhere(matching_matrix == max)  
        words_=np.array(words)
        return " ".join(words_[index_max[:,1]])
        print(sentence_list[index_max[:,0]],"corresponds to",words_[index_max[:,1]])
    else:
        return None

"""match_semantic(actions,sentence)
match_semantic(attributes,sentence)
match_semantic(objects,sentence)"""

if __name__=="__main__":
    sentence = "blink the bedroom light"

    light_1=connected_object('light')
    light_1.set_attribute('kitchen')
    light_1.set_action('turnoff',lambda :print("turnoff kitchen light"))
    light_1.set_action('turnon',lambda :print("turnon kitchen light"))
    light_1.set_action('blink',lambda :print("blink kitchen light"))

    light_2=connected_object('light')
    light_2.set_attribute('bedroom')
    light_2.set_action('turnoff',lambda :print("turnoff bedroom light"))
    light_2.set_action('turnon',lambda :print("turnon bedroom light"))
    light_2.set_action('blink',lambda :print("blink bedroom light"))

    home_network=objects_network(OBJECTS,ATTRIBUTES,ACTIONS)

    home_network.add_obj(light_1)
    home_network.add_obj(light_2)

    home_network.trigger_object(
    match_semantic(OBJECTS,sentence),
    match_semantic(ATTRIBUTES,sentence),
    match_semantic(ACTIONS,sentence))
