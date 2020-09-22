import {shallowMount} from "@vue/test-utils";
import ActivitySearch from "../../src/components/Search/ActivitySearch";
import store from "../../src/store";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(ActivitySearch, {
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

describe('ActivitySearch.vue', () => {
    test('Search parameters are correctly formatted without specified activity types', () => {
        const distance = 50;
        wrapper.vm.maxDistance = distance;
        const latLngObject = {lat: -43.517672, lng: 172.571489};
        wrapper.vm.profileLocationLatLong = latLngObject;
        wrapper.vm.chosenActivityTypes = [];
        wrapper.vm.activitySearchType = "all";
        expect(wrapper.vm.getSearchParameters()).toStrictEqual({distance: 50000, latitude: -43.517672, longitude: 172.571489})
    })
});