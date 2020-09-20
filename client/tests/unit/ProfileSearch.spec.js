import { shallowMount } from '@vue/test-utils'
import ProfileSearch from "../../src/components/Search/ProfileSearch";

let wrapper;
const DEFAULT_RESULT_COUNT = 10

beforeEach(() => {
    wrapper = shallowMount(ProfileSearch, {
        propsData: {},
        mocks: {},
        stubs: {},
        methods: {}
    });
});

afterEach(() => {
    wrapper.destroy();
});

describe('ProfileSearch.vue', () => {
    test('Empty search results in default search parameters', () => {
        wrapper.vm.profile = {"fitness_statement": 3}
        wrapper.vm.name = ""
        wrapper.vm.email = ""
        expect(wrapper.vm.getSearchParameters()).toStrictEqual({startIndex: 0, count: DEFAULT_RESULT_COUNT})
    });
    test('Search with name has default parameters and name', () => {
        wrapper.vm.profile = {"fitness_statement": 3}
        const name = "Frank"
        wrapper.vm.name = name
        wrapper.vm.email = ""
        expect(wrapper.vm.getSearchParameters()).toStrictEqual({startIndex: 0, count: DEFAULT_RESULT_COUNT, fullname: name})
    });
    test('Search with email has default parameters and email', () => {
        wrapper.vm.profile = {"fitness_statement": 3}
        const email = "beans@gmail.com"
        wrapper.vm.name = ""
        wrapper.vm.email = email
        expect(wrapper.vm.getSearchParameters()).toStrictEqual({startIndex: 0, count: DEFAULT_RESULT_COUNT, email: email})
    });
});
