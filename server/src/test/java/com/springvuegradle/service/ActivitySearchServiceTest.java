package com.springvuegradle.service;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@DataJpaTest
public class ActivitySearchServiceTest {

    @Autowired
    private ActivitySearchService activitySearchService;

    @Test
    void inRangeSucceedsWithValidNumbersTest(){
        assertTrue(activitySearchService.isInRange(7D, 5, 10));
    }

    @Test
    void inRangeFailsWithNullTest(){
        assertFalse(activitySearchService.isInRange(null, 4, 15));
    }

    @Test
    void inRangeFailsWithOutOFRangeTest() {
        assertFalse(activitySearchService.isInRange(33D, 4, 15));
    }

    @Test
    void inRangeSuceedsWithBoundaryTest(){
        assertTrue(activitySearchService.isInRange(5D, 5, 10));
    }

    @Test
    void userCanSeeActivityInRangeTest(){

    }

    @Test
    void userCannotSeeActivityOutOfRangeTest(){

    }

    @Test
    void distanceBetweenSameLocationsTest(){
        assertEquals(0, activitySearchService.distance(50, 50, 50 ,50));
    }

    @Test
    void distanceBetweenDifferentLocationsTest(){
        assertThat(activitySearchService.distance(10, 30, 30 ,50)).isBetween(3040600D, 3040610D);
    }
}
