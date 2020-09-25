import {shallowMount} from "@vue/test-utils";
import ActivitySearchByLocation from "../../src/components/Search/ActivitySearchByLocation";
import store from "../../src/store";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(ActivitySearchByLocation, {
        propsData: {},
        mocks: {},
        data: function(){
            return {
                profile: {location: {address: "hello"}},
                geocoder: {},
                maxDistance: 50,
                activitySearchType: "all",
                chosenActivityTypes: [],
                activityResults: [],
                store: store,
                profileLocationLatLong: {},
                locationString: "",
                searchResultString: "Please click the 'Search' button below!"
            }
        },
        stubs: {},
        methods: {}
    });
});

afterEach(() => {
    wrapper.destroy();
});

describe('ActivitySearchByLocation.vue', () => {
    test('Search parameters are correctly formatted without specified activity types', () => {
        const distance = 50;
        wrapper.vm.maxDistance = distance;
        const latLngObject = {lat: -43.517672, lng: 172.571489};
        wrapper.vm.profileLocationLatLong = latLngObject;
        wrapper.vm.chosenActivityTypes = [];
        wrapper.vm.activitySearchType = "all";
        expect(wrapper.vm.getSearchParameters()).toStrictEqual({distance: 50000, latitude: -43.517672, longitude: 172.571489})
    });
    test('An activity with a date has their details correctly formatted for the information window', () => {
        const activity = {
            activityName: "Fun stuff",
            activityTypes: ["tennis"],
            continuous: false,
            endTime: "2020-09-26T11:00:00+12:00",
            id: 70001,
            latitude: 38.8894587,
            location: "Lincoln Memorial Bookstore, Washington, DC, USA",
            longitude: -77.0499817,
            startTime: "2020-09-24T23:00:00+12:00"
        }

        wrapper.vm.formattedStartTime = "23:00 24/09/2020"
        wrapper.vm.formattedEndTime = "11:00 26/09/2020"
        wrapper.vm.activityTypesString =
            `<h1 style="font-size: 16px; font-weight: bold"> Activity Types:</h1>` +
            `<span style="color:red; font-weight: bold">*</span><span> ${activity.activityTypes[0]} </span>`
        const expectedFormat =
            `<div>` +
            `<h1 style="font-size: 22px; font-weight: bold; font-style: italic">${activity.activityName}</h1>` +
            `<h1 style="font-weight: bold">${activity.location}</h1>` +
            `<br>` +
            `<h1 style="font-weight: bold">Start date and time: <span>23:00 24/09/2020</span></h1>` +
            `<br>` +
            `<h1 style="font-weight: bold">End date and time: <span>11:00 26/09/2020</span></h1>` +
            `<br>` +
            `<h1 style="font-size: 16px; font-weight: bold"> Activity Types:</h1>` +
            `<span style="color:red; font-weight: bold">*</span><span> ${activity.activityTypes[0]} </span>`+
            `</div>`
        expect(wrapper.vm.formatActivityDetails(activity)).toEqual(expectedFormat)
    });
    test('An activity without a date has their details correctly formatted for the information window', () => {
        const activity = {
            activityName: "Fun stuff",
            activityTypes: ["tennis"],
            continuous: false,
            endTime: null,
            id: 70001,
            latitude: 38.8894587,
            location: "Lincoln Memorial Bookstore, Washington, DC, USA",
            longitude: -77.0499817,
            startTime: null
        }
        wrapper.vm.activityTypesString =
            `<h1 style="font-size: 16px; font-weight: bold"> Activity Types:</h1>` +
            `<span style="color:red; font-weight: bold">*</span><span> ${activity.activityTypes[0]} </span>`
        const expectedFormat =
            `<div>` +
            `<h1 style="font-size: 22px; font-weight: bold; font-style: italic">${activity.activityName}</h1>` +
            `<h1 style="font-weight: bold">${activity.location}</h1>` +
            `<br>` +
            `<h1 style="font-weight: bold">Start date: Now!</h1>` +
            `<br>` +
            `<h1 style="font-weight: bold">End date: "${activity.activityName}" is continuous!</h1>` +
            `<br>` +
            `<h1 style="font-size: 16px; font-weight: bold"> Activity Types:</h1>` +
            `<span style="color:red; font-weight: bold">*</span><span> ${activity.activityTypes[0]} </span>`+
            `</div>`
        expect(wrapper.vm.formatActivityDetails(activity)).toEqual(expectedFormat)
    })
});