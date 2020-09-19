import { shallowMount } from '@vue/test-utils'
import ActivityTypesField from "../../src/components/Activities/ActivityHelpers/ActivityTypesField";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(ActivityTypesField, {
        propsData: {},
        mocks: {},
        stubs: {},
        methods: {}
    });
});

afterEach(() => {
    wrapper.destroy();
});

describe('ActivityTypesField.vue', () => {
    test('Activity type filtering works when items are present in the list', () => {
        const activityTypes = ["Running", "Ruby", "Rug", "Football", "Hiking", "Soccer"]
        wrapper.vm.possibleActivityTypes = activityTypes
        wrapper.vm.getFilteredActivityTypes("Ru")
        expect(wrapper.vm.filteredActivityTypes).toEqual(["Running", "Ruby", "Rug"])
    });
    test('Activity type filtering works when no items are present in the list', () => {
        const activityTypes = ["Running", "Ruby", "Rug", "Football", "Hiking", "Soccer"]
        wrapper.vm.possibleActivityTypes = activityTypes
        wrapper.vm.getFilteredActivityTypes("Tennis")
        expect(wrapper.vm.filteredActivityTypes).toStrictEqual([])
    });
});
