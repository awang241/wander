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
    // needs to be implemented once reusable autocomplete component has been merged
});
