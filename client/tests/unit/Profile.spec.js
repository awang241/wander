import { shallowMount } from '@vue/test-utils'
import Profile from "../../src/components/Profile";

let wrapper;
const $route = {
    params: {
        id: 1
    }
};

beforeEach(() => {
    wrapper = shallowMount(Profile, {
        propsData: {},
        mocks: {
            $route
        },
        stubs: {},
        methods: {},
        routes: {},
        data() {
            return {
                profile: {activities : [], passports: []}
            }
        }
    });

});

afterEach(() => {
    wrapper.destroy();
});

describe('Profile.vue', () => {
    test('Computed fitness statement property works given a fitness level', () => {
        wrapper.vm.profile = {"fitness": 3}
        expect(wrapper.vm.fitnessStatement).toContain("Advanced");
    });
    test('Default fitness statement as beginner when fitness level not given', () => {
        expect(wrapper.vm.fitnessStatement).toContain("Beginner")
    });
    test('viewing on profile is false when the user is not viewing their profile', () => {
        wrapper.vm.profile = {"id": 1}
        expect(wrapper.vm.viewingOwnProfile).toBe(false)
    });
})