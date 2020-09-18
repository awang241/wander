import { shallowMount } from '@vue/test-utils'
import ActivitySearch from "../../src/components/Search/ActivitySearch";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(ActivitySearch, {
        propsData: {},
        mocks: {},
        stubs: {},
        methods: {}
    });
});

afterEach(() => {
    wrapper.destroy();
});

describe('ActivitySearch.vue', () => {
    test('Search with max distance has default parameters and email', () => {
        const max_distance = 60
        wrapper.vm.latitude = 50
        wrapper.vm.longitude = 50
        wrapper.vm.chosenActivityTypes = []
        wrapper.vm.maxDistance = max_distance
        expect(wrapper.vm.getSearchParameters()).toStrictEqual({latitude: 50, longitude: 50, distance: max_distance})
    });
});
