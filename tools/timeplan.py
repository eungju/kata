#!/bin/env python
# -*- coding: utf-8 -*-

import re

class Computer:
    def __init__(self):
        self.rules = [
            (r"(\d{2}):(\d{2})", self.reset),
            (r".*\s+(\d+)m", self.activity),
            (r"ELAPSED\?$", self.elapsed),
            (r".*", self.bypass)]
    def hours_and_minutes(self, minutes):
        return (minutes / 60, minutes % 60)
    def reset(self, m):
        self.start = int(m.group(1)) * 60 + int(m.group(2))
        self.now = self.start
    def activity(self, m):
        duration = int(m.group(1))
        print "%02d:%02d" % self.hours_and_minutes(self.now), m.group(0)
        self.now += duration
    def elapsed(self, m):
        elapsed = self.now - self.start
        print "%dh %dm" % self.hours_and_minutes(elapsed)
    def bypass(self, m):
        print m.group(0)
    def evaluate(self, data):
        for line in data.strip().split("\n"):
            instruction = line.strip();
            for (p, r) in self.rules:
                m  = re.match(p, instruction)
                if m:
                    r(m)
                    break
        
PLAN = """
Day One
10:00
Introduction 10m
Handson 20m
Break 10m
ELAPSED?
"""
Computer().evaluate(PLAN)
