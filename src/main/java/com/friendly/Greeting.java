package com.friendly;

public class Greeting {
    private final String defaultGreeting;
    public Greeting(String defaultGreeting) {
        this.defaultGreeting = defaultGreeting;
    }
    public Greeting() {
        this("Hi Mate.");
    }
    public void say() {
        say(defaultGreeting);
    }
    public void say(String greeting) {
        System.out.println(greeting);
    }
}