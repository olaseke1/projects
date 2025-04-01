import pygame
from utilis import WINDOW_HEIGHT, WINDOW_WIDTH
class Snake:
    def __init__(self,colour,direction,x_position,y_position):
        self.colour = colour
        self.direction = direction
        self.x_position = x_position
        self.y_position = y_position
        self.body= [(x_position,y_position)]
        self.score = 0
    
    def draw(self,display):
        #DRAWS THE SNACK BODY SHAPE
        for x,y in self.body:
            pygame.draw.rect(display,self.colour,[x,y,30,30])
    def move(self):
        #MOVES THE SNAKE BODY, CHECKS FOR BOUNDARY FIRST THO
        new_x = self.x_position + self.direction[0]
        new_y = self.y_position + self.direction[1]
        if (new_x >= 10 and new_x <= WINDOW_WIDTH - 30) and (new_y >= 10 and new_y <= WINDOW_HEIGHT - 30):
             self.x_position = new_x
             self.y_position = new_y
             self.body.insert(0, (self.x_position, self.y_position))
             self.body.pop()
    def increase(self):
        #INCREASES SIZE OF SNAKE
        self.score += 10
        new_x = self.x_position + self.direction[0]
        new_y = self.y_position + self.direction[1]
        if (new_x >= 10 and new_x <= WINDOW_WIDTH - 30) and (new_y >= 10 and new_y <= WINDOW_HEIGHT - 30):
            self.x_position = new_x
            self.y_position = new_y
            self.body.insert(0, (self.x_position, self.y_position))
