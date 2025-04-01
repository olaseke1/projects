import pygame
from utilis import WINDOW_HEIGHT, WINDOW_WIDTH
import random
class Food:
    def __init__(self):
        self.food_x = random.randrange(30,WINDOW_WIDTH-30,30)
        self.food_y = random.randrange(30,WINDOW_HEIGHT-30,30)
        self.food = pygame.transform.scale(pygame.image.load("food.png"),(30,30))

    def draw(self, display):
        display.blit(self.food,(self.food_x,self.food_y))