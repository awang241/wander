import { shallowMount } from '@vue/test-utils';
import EditLocation from "../../src/components/Profile/EditProfile/EditLocation";
let wrapper;

beforeEach(() => {
    wrapper = shallowMount(EditLocation, {
        propsData: {profile: {location: {address: "here", latitude: 1, longitude: 2}}},
        mocks: {},
        stubs: {},
        methods: {}
    });
});

afterEach(() => {
    wrapper.destroy();
});

describe('EditLocation.vue', () => {
    test('has two divs', () => {
        expect(wrapper.findAll('div').length).toBe(2);
    });

    test('updates location string', () => {
        wrapper.vm.profile.location.address = "a"
        wrapper.vm.locationString = "b"
        wrapper.vm.updateLocationString()
        expect(wrapper.vm.locationString == wrapper.vm.profile.location.address)
    });

    test('updates location string from autocomplete', () => {
        wrapper.vm.locationString = "b"
        wrapper.vm.profileLocationLatLong= {lat: 0, lng: 0}
        wrapper.vm.updateMapLocationFromAutoComplete(location)
        expect(wrapper.vm.locationString == wrapper.vm.profile.location.address)
        expect(wrapper.vm.profileLocationLatLong == {lat: 1, lng: 2})
    });
});