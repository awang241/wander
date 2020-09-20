import { shallowMount } from '@vue/test-utils'
import EditActivityTypes from "../../src/components/Profile/EditProfile/EditActivityTypes";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(EditActivityTypes, {
        propsData: {
            "profile":{"activities": []}
        },
        mocks: {},
        stubs: {},
        methods: {},
    });
    wrapper.vm.warningToast = jest.fn();
});

afterEach(() => {
    wrapper.destroy();
});

describe('EditActivityTypes.vue', () => {
        test('Chosen activity types start empty for a profile', () => {
           wrapper.vm.chosenActivityTypes = []
           expect(wrapper.vm.chosenActivityTypes).toStrictEqual([]);
        });
        test('Adding an activity type', () => {
            wrapper.vm.chosenActivityTypes = []
            wrapper.vm.newActivityType = "Football"
            wrapper.vm.addActivityType()
            expect(wrapper.vm.chosenActivityTypes).toStrictEqual(["Football"]);
        });
        test('Adding an activity type that is already chosen', () => {
            wrapper.vm.chosenActivityTypes = ["Football"]
            wrapper.vm.newActivityType = "Football"
            wrapper.vm.addActivityType()
            expect(wrapper.vm.chosenActivityTypes).toStrictEqual(["Football"]);
        });
        test('Deleting an activity type', () => {
            wrapper.vm.chosenActivityTypes = ["Football"]
            wrapper.vm.deleteActivityType("Football")
            expect(wrapper.vm.chosenActivityTypes).toStrictEqual([]);
        });
        test('Add an activity type with no name', () => {
            wrapper.vm.chosenActivityTypes = []
            wrapper.vm.newActivityType = ""
            wrapper.vm.addActivityType()
            expect(wrapper.vm.chosenActivityTypes).toStrictEqual([]);
        });
})

